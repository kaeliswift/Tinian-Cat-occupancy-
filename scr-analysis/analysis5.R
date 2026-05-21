#Cleaning of data formatting & mask formation
#5/20/2026

library(secr)
library(tidyverse)
library(lubridate)
library(sf)
library(sp)
library(ggplot2)
library(readr)
library(terra)

#Load TrapTagger Data ##########################################################
raw.data <- read.csv("TrapTagger_Cat_Individuals.csv")

#Removing None & unidentifiable detection & separating | detections
data <- raw.data %>%
  filter(Individuals != "None") %>%
  mutate(Individuals = str_trim(Individuals)) %>%
  separate_rows(Individuals, sep = "\\|") %>%
  filter(Individuals != "unidentifiable") %>%
  mutate(DateTime = mdy_hm(Timestamp, tz = "Pacific/Guam"))

#Load Deployment Data ##########################################################
start_end <- read.csv("Camera Depolyment and Termination.csv") %>%
  filter(Site.name != "") %>%
  rename(Site.Name = Site.name)

#Manual fixes for problem children cameras......
#problem sites: J6, J4, G41
#force 1st end date of J4 as 11/15/24
start_end[7, "Deployment"] <- "11/02/2024"
#force 1st end date of J6 to 11/15/24
start_end[10, "Termination"] <- "11/15/2024"
#G41 good as is

#Format time data and add Session..............
start_end <- start_end %>%
  mutate(
    Deployment = mdy_hms(paste(Deployment, "00:00:00"), tz = "Pacific/Guam"),
    Termination = mdy_hms(paste(Termination, "23:59:59"), tz = "Pacific/Guam"),
    Session = ifelse(year(Deployment) == 2024, 1, 2)
  )

#Filter cat data to within study period ########################################
problem_children <- start_end[c(8,11,19),] #filters for J6,J4,G41
first_set <- start_end[-c(8,11,19),]

data.first_set <- data %>%
  left_join(first_set, by = "Site.Name") %>%
  filter(DateTime >= Deployment & DateTime <= Termination) %>%
  distinct(File, .keep_all = TRUE)

data.prob_child <- data %>%
  left_join(problem_children, by = "Site.Name") %>%
  filter(DateTime >= Deployment & DateTime <= Termination) %>%
  distinct(File, .keep_all = TRUE)

data <- rbind(data.first_set, data.prob_child)

#Define Session & Clean detections to > 30 min apart ###########################

data <- data %>%
  mutate(
    Session = ifelse(year(DateTime) == 2024, 1, 2),
    Animal = Individuals 
  )

#Filters to detection > 30 min apart (clusters are 30 min apart)...................
data <- data %>%
  arrange(Cluster.ID, Individuals, DateTime) %>%
  group_by(Site.Name, Cluster.ID, Individuals) %>%
  slice(1) %>%
  ungroup()

# Session-wide Occasions #######################################################
# occasion start dates are the same per session
session_starts <- tibble(
  Session = c(1, 2),
  SessionStart = as.POSIXct(
    c("2024-10-17 00:00:00",
      "2025-04-24 00:00:00"),
    tz = "Pacific/Guam"
  )
)

# create occasions per session................
data <- data %>%
  left_join(session_starts, by = "Session") %>%
  mutate(
    Occasion = floor(as.numeric(difftime(DateTime, SessionStart, units = "days")) / 7) + 1
  ) %>%
  filter(Occasion >= 1 & Occasion <= 6)

# Define TrapID ################################################################
data <- data %>%
  mutate(TrapID = Site.Name)

# Format Trap Data #############################################################
traps.data <- read.csv("cat_cam_deployment_landcover_type.csv") %>%
  rename(TrapID = Label, x = Longitude, y = Latitude) %>%
  mutate(TrapID = str_trim(TrapID))

# transforming x,y coordinates to secr format 
traps.sf <- st_as_sf(traps.data, coords = c("x", "y"), crs = 4326) %>%
  st_transform(crs = 32655)

coords <- st_coordinates(traps.sf)

traps.data <- traps.data %>%
  mutate(x = coords[,1], y = coords[,2])

# creating separate trapfiles per session
trapIDs_year1 <- start_end %>% filter(Session == 1) %>% pull(Site.Name)
trapIDs_year2 <- start_end %>% filter(Session == 2) %>% pull(Site.Name)

traps_year1 <- traps.data %>% filter(TrapID %in% trapIDs_year1) %>% select(TrapID, x, y)
traps_year2 <- traps.data %>% filter(TrapID %in% trapIDs_year2) %>% select(TrapID, x, y)



# Create txt Files #############################################################
capt <- data %>% select(Session, Animal, Occasion, TrapID)

# all capture history data
write.table(capt, "capt_all.txt",
            row.names = FALSE, col.names = FALSE,
            quote = FALSE, sep = "\t")

# trap info session 1
write.table(traps_year1, "traps_year1.txt",
            row.names = FALSE, col.names = FALSE,
            quote = FALSE, sep = "\t")

# trap info session 2
write.table(traps_year2, "traps_year2.txt",
            row.names = FALSE, col.names = FALSE,
            quote = FALSE, sep = "\t")

# Create Capture History #######################################################
ch <- read.capthist(
  captfile = "capt_all.txt",
  trapfile = list("traps_year1.txt", "traps_year2.txt"),
  detector = "count",
  fmt = "trapID"
) # should say: no errors found :-)


# Add CLASS.landcover covariate to Traps (site-specific covariate) #############
# create traps object for secr
traps1 <- traps(ch[[1]])
traps2 <- traps(ch[[2]])

traps_year1 <- traps.data %>% filter(TrapID %in% trapIDs_year1) %>% select(TrapID, x, y, CLASS.landcover)
traps_year2 <- traps.data %>% filter(TrapID %in% trapIDs_year2) %>% select(TrapID, x, y, CLASS.landcover)


covariates(traps1) <- data.frame(site_habitat = traps_year1$CLASS.landcover)
covariates(traps2) <- data.frame(site_habitat = traps_year2$CLASS.landcover)

traps(ch[[1]]) <- traps1
traps(ch[[2]]) <- traps2

covariates(traps(ch[[1]])) #worked
covariates(traps(ch[[2]]))


# Inspect ch ###################################################################
summary(ch)
plot(ch, tracks = TRUE)

usage(traps(ch[[1]]))[1:42, ] #currently no usage -> ran into errors due to full usage in session 2& partial usage in session 1
usage(traps(ch[[2]]))[1:8, ]                      # session 1 is still mainly covered so will ignore usage for now 

# Rough estimate of buffer size ################################################
#estimate of sigma HN to suggest buffer size
#buffer size is usually 4 sigma HN
RPSV(ch, CC = TRUE)
#session 1 = 1773 * 4 = 7092
#session 2 = 697 * 4 = 2788

# different estimate of buffer for HN
suggest.buffer(ch, detectfn = 'HN', RBtarget = 0.001)
# session 1 = 7790
# session 2 = 3160

#run the null model with a half normal detection
cats.HN7000 <-secr.fit(ch, buffer=7000, trace = FALSE)

cats.HN7000 

#plot buffer width (m) x n/esa(buffer)ha
par(pty = "s",mar = c(4,4,2,2),mgp =c(2.5,0.8,0),las =1)
esaPlot(cats.HN7000,ylim =c(0,4))
abline(v=7000, col ="red",lty =2)

suggest.buffer(cats.HN7000) #both ~11000 m -> weird



#Checking D estimates & changes in buffer size #################################
#OK... next moves before models......
#graph buffer sizes & D estimates
#decide on final buffer

#Exploring buffer sizes
buffers <- c(3000, 5000, 7000, 9000, 11000) #checking 3 km to 11 km 

#read in shapefile data
tinian <- st_read(
  "C:/Users/celin/Tinian-Cat-occupancy-/scr-analysis/CNMI Hi-Res veg data/tinian_release.shp"
)

# make sure CRS matches traps
st_crs(tinian)

# dissolve polygons into single island boundary
island_boundary <- st_union(tinian)

# run function that changes buffer to calculate several D estimates
fits <- lapply(buffers, function(b) { #THIS WILL TAKE A LOT OF TIME 
  
  mask_b <- make.mask(
    traps(ch),
    buffer = b,
    spacing = 250, #can change this
    type = "trapbuffer", #restricts the grid to points within distance buffer of any detector.
    poly = vect(island_boundary)
  )
  
  secr.fit(
    ch,
    mask = mask_b,
    detectfn = 0   # half normal
  )
})


saveRDS(fits, file = "D_buffer_fits.rds")
fits <- readRDS("D_buffer_fits.rds")


# extract density values (D) from fits
D_values <- sapply(fits, function(fit) {
  sapply(derived(fit), function(x) x["D","estimate"])
})

#plot estimates vs buffer size
D_df <- as.data.frame(t(D_values))
colnames(D_df) <- c("Session1", "Session2")

D_df$buffer <- buffers

D_long <- D_df %>%
  pivot_longer(cols = starts_with("Session"),
               names_to = "Session",
               values_to = "D")

ggplot(D_long, aes(x = buffer, y = D, color = Session)) +
  geom_line() +
  geom_point() +
  labs(title = "Density vs Buffer Size",
       x = "Buffer (m)",
       y = "Density (D)") +
  theme_minimal()


#separate session plots: PAY CLOSE ATTENTION TO Y-VALUES
ggplot(subset(D_long, Session == "Session1"),
       aes(x = buffer, y = D)) +
  geom_line() +
  geom_point() +
  labs(title = "Session 1: Density vs Buffer Size",
       x = "Buffer (m)",
       y = "Density (D)") +
  theme_minimal()

ggplot(subset(D_long, Session == "Session2"),
       aes(x = buffer, y = D)) +
  geom_line() +
  geom_point() +
  labs(title = "Session 2: Density vs Buffer Size",
       x = "Buffer (m)",
       y = "Density (D)") +
  theme_minimal()


#checking if D estimate stabilizes by looking at values

#session 1
sapply(fits, function(fit) {
  derived(fit)[[1]]["D","estimate"]}) #shows D estimates
#session 2 
sapply(fits, function(fit) {
  derived(fit)[[2]]["D","estimate"]}) #shows D estimates

#both sessions 
sapply(fits, function(fit) {sapply(derived(fit), function(x) x["D","estimate"])})

#graphing
D_values <- sapply(fits, function(fit) {
  sapply(derived(fit), function(x) x["D","estimate"])
})

matplot(buffers, t(D_values), type = "b", pch = 1:2, col = 1:2,
        xlab = "Buffer (m)", ylab = "D estimate", lty = 1:2)
legend("topright", legend=c("Session 1","Session 2"), pch=1:2, lty=1:2)

#Choosing Buffer = 7000 for now!!!!!!!!


# Set Mask and Analysis ########################################################

# 1. read shapefile................
tinian <- st_read(
  "C:/Users/celin/Tinian-Cat-occupancy-/scr-analysis/CNMI Hi-Res veg data/tinian_release.shp"
)

# make sure CRS matches traps
st_crs(tinian)

# dissolve polygons into single island boundary
island_boundary <- st_union(tinian)

plot(st_geometry(island_boundary)) #shows island boundary

# 2. reclassify habitat...............
tinian <- tinian %>%
  mutate(
    habitat = case_when(
      grepl("Leucaena", CLASS) ~ "tangantangan",
      grepl("Mixed", CLASS) ~ "mixed_introduced",
      grepl("Casuarina", CLASS) ~ "ironwood",
      grepl("Other", CLASS) ~ "shrub_grass",
      grepl("Native", CLASS) ~ "native_limestone",
      TRUE ~ NA_character_
    )
  )

table(tinian$habitat, useNA = "ifany")

# 3. rasterize habitat...................
tinian_v <- vect(tinian)

# convert habitat to factor
tinian_v$habitat <- as.character(tinian_v$habitat)

# template raster
r <- rast(tinian_v,
  resolution = 250 #can change this
)

# rasterize
habitat_raster <- rasterize(
  tinian_v,
  r,
  field = "habitat",
)

habitat_raster <- rasterize(
  tinian_v,
  r,
  field = "habitat"
)

plot(habitat_raster) #nice

# 4. build session masks separately...........

#define shoreline
shoreline <- st_boundary(island_boundary)

# create masks per session w/ covariates
masklist <- lapply(
  1:2,
  function(i){
    
    m <- make.mask(
      traps(ch[[i]]),
      type = "trapbuffer",
      buffer = 7000,
      spacing = 250,
      poly = vect(island_boundary)
    )
    
    # coordinates
    xy <- cbind(m$x, m$y)
    
    # -------------------------
    # HABITAT
    # -------------------------
    
    ex <- terra::extract(
      habitat_raster,
      xy
    )
    
    covariates(m)$habitat <- factor(ex$habitat)
    
    covariates(m)$habitat <-
      relevel(
        covariates(m)$habitat,
        ref = "native_limestone"
      )
    
    # -------------------------
    # DISTANCE TO SHORE
    # -------------------------
    
    # convert mask points to sf
    pts <- st_as_sf(
      data.frame(x = m$x, y = m$y),
      coords = c("x", "y"),
      crs = st_crs(tinian)
    )
    
    # calculate distance to shoreline
    dshore <- st_distance(
      pts,
      shoreline
    )
    
    # convert units matrix to numeric vector
    covariates(m)$d.to.shore <- as.numeric(dshore)
    
    m
  }
)

summary(covariates(masklist[[1]])$d.to.shore)

summary(covariates(masklist[[2]])$d.to.shore)

summary(masklist[[1]])
summary(masklist[[2]])

verify(masklist[[1]]) #good
verify(masklist[[2]]) #good
verify(masklist) #issue

names(covariates(masklist[[1]]))
names(covariates(masklist[[2]]))


# 5. replace blanks in habitat with NA............
for(i in 1:2){
  
  m <- masklist[[i]]
  
  h <- as.character(covariates(m)$habitat)
  
  # convert blanks to NA
  h[h == ""] <- NA
  
  # rebuild factor
  h <- factor(h)
  
  # set reference level -> needs to be dominant category
  h <- relevel(h, ref = "tangantangan")
  
  covariates(m)$habitat <- h
  
  masklist[[i]] <- m
}

table(covariates(masklist[[1]])$habitat, useNA="ifany")
table(covariates(masklist[[2]])$habitat, useNA="ifany")


# 6. replace NAs with nearest neighbor..........
#session 1...
m <- masklist[[1]]

h <- covariates(m)$habitat

# replace NA with nearest category manually
h[is.na(h)] <- names(sort(table(h), decreasing = TRUE))[1]

covariates(m)$habitat <- droplevels(h)

masklist[[1]] <- m

#check it worked
table(
  covariates(masklist[[1]])$habitat,
  useNA = "ifany"
) #good

#session 2....
m <- masklist[[2]]

h <- covariates(m)$habitat

# replace NA with nearest category manually
h[is.na(h)] <- names(sort(table(h), decreasing = TRUE))[1]

covariates(m)$habitat <- droplevels(h)

masklist[[2]] <- m


# 6. force secr to recognize both session masks.............
class(masklist) <- c("mask", "list")
verify(masklist)

# 7. graph mask.........................
# raw data
plot(masklist[[1]], covariate = "habitat", pch = 15, cex = 0.6)
plot(traps(ch[[1]]), add = TRUE, pch = 16)

plot(masklist[[1]], covariate = "d.to.shore", pch = 15, cex = 0.6)
plot(traps(ch[[1]]), add = TRUE, pch = 16)

#other mask trials.... includes trapbuffer
plot(masklist, border = 100, ppoly = FALSE)
plot(masklist[[1]],
     covariate = "habitat",
     pch = 15,
     cex = 1.2,
     add = TRUE)
plot(traps(ch[[1]]), add = TRUE, pch = 16)

#session 2
plot(masklist, border = 100, ppoly = FALSE)
plot(masklist[[2]],
     covariate = "habitat",
     pch = 15,
     cex = 1.2, 
     add = TRUE)

plot(traps(ch[[2]]), add = TRUE, pch = 16)

#plot mask extent
plot(
  masklist[[1]]$x,
  masklist[[1]]$y,
  col = NA
)
plot(masklist[[1]], covariate = "d.to.shore", pch = 15, cex = 0.6, add =TRUE)
plot(traps(ch[[1]]), add = TRUE)

plot(st_geometry(island_boundary), col = "grey90", border = "grey40")
plot(masklist[[1]], covariate = "habitat", add = TRUE, pch = 15, cex = 0.5)
plot(traps(ch[[1]]), add = TRUE, pch = 16)
plot(traps(ch[[2]]), add = TRUE, pch = 16)

# 8. save mask..............
saveRDS(masklist, file = "masklist.rds")
masklist <- readRDS("masklist.rds")

# 9. try modelling..............
# null model
m0 <- secr.fit(
  ch,
  mask = masklist,
  model = list(
    D ~ 1,
    g0 ~ 1,
    sigma ~ 1
  ),
  detectfn = "halfnormal"
)

summary(m0)
AIC(m0) #1430

saveRDS(m0, file = "m0.rds")
m0 <- readRDS("m0.rds")

# null model with hazard rate
m0HR <- secr.fit(
  ch,
  mask = masklist,
  model = list(
    D ~ 1,
    g0 ~ 1,
    sigma ~ 1
  ),
  detectfn = 1 # hazard rate
)

summary(m0HR)
AIC(m0, m0HR) 

# null model with exponential
m0EX <- secr.fit(
  ch,
  mask = masklist,
  model = list(
    D ~ 1,
    g0 ~ 1,
    sigma ~ 1
  ),
  detectfn = 2 #exponential
)

summary(m0EX)
AIC(m0, m0HR, m0EX) #HR is best preforming detection rate but using HN for now because its estimates look better

# continuing on with half normal detection rate for now 
# habitat model
table(covariates(masklist[[1]])$habitat)
table(covariates(masklist[[2]])$habitat) #largest value needs to be reference level -> tangantangan

mDhabitat <- secr.fit(
  ch,
  mask = masklist,
  model = list(
    D ~ habitat,
    g0 ~ 1,
    sigma ~ 1
  ),
  detectfn = "halfnormal"
)

summary(mDhabitat)
AIC(m0, mDhabitat) #habitat is better by 7 AIC

saveRDS(mDhabitat, file = "mDhabitat.rds")
mDhabitat <- readRDS("mDhabitat.rds")

#try graphing
hold=predictDsurface(mDhabitat, mask = masklist, se.D = FALSE, cl.D = FALSE, alpha =0.05)
plot(hold)   #this one is boring cause we have no variation on D


# session model 
mDsession <- secr.fit(
  ch,
  mask = masklist,
  model = list(
    D ~ session,
    g0 ~ 1,
    sigma ~ 1
  ),
  detectfn = "halfnormal"
)

summary(mDsession)
AIC(m0, mDhabitat, mDsession) #worse than the null model

saveRDS(mDsession, file = "mDsession.rds")
mDsession <- readRDS("mDsession.rds")

# distance to shore model
#first scale the covariate
for(i in 1:2){
  covariates(masklist[[i]])$d.to.shore <-
    scale(covariates(masklist[[i]])$d.to.shore)
}

mDshore <- secr.fit(
  ch,
  mask = masklist,
  model = list(
    D ~ d.to.shore,
    g0 ~ 1,
    sigma ~ 1
  ),
  detectfn = "halfnormal"
)

summary(mDshore)
AIC(m0, mDhabitat, mDsession, mDshore) #best model so far

saveRDS(mDshore, file = "mDshore.rds")
mDshore <- readRDS("mDshore.rds")

#try graphing
hold2=predictDsurface(mDhabitat, mask = masklist, se.D = FALSE, cl.D = FALSE, alpha =0.05)
plot(hold2)  


# Create effort matrix function ################################################
make_usage_matrix <- function(session_num, traps_df, deploy_df, n_occasions = 6) {
  
  trap_ids <- traps_df$TrapID
  deploy_sess <- deploy_df %>% filter(Session == session_num)
  
  usage_mat <- matrix(
    0,
    nrow = length(trap_ids),
    ncol = n_occasions,
    dimnames = list(trap_ids, paste0("occ", 1:n_occasions))
  )
  
  session_start <- min(deploy_sess$Deployment)
  
  for(i in seq_along(trap_ids)) {
    
    trap_info <- deploy_sess %>%
      filter(Site.Name == trap_ids[i])
    
    if(nrow(trap_info) == 0) next
    
    for(k in 1:n_occasions) {
      
      occ_start <- session_start + days((k - 1) * 7)
      occ_end   <- occ_start + days(7) - seconds(1)
      
      total_overlap <- 0
      
      for(j in 1:nrow(trap_info)) {
        
        overlap_start <- max(trap_info$Deployment[j], occ_start)
        overlap_end   <- min(trap_info$Termination[j], occ_end)
        
        overlap_days <- as.numeric(difftime(overlap_end, overlap_start, units = "days"))
        overlap_days <- max(0, overlap_days)
        
        total_overlap <- total_overlap + overlap_days
      }
      
      usage_mat[i, k] <- round(min(1, total_overlap / 7), 3)
    }
  }
  
  usage_mat
}

# Build Usage/Effort Matrices ##################################################

usage_year1 <- make_usage_matrix(1, traps_year1, start_end)
usage_year2 <- make_usage_matrix(2, traps_year2, start_end)


# Attach Usage/Effort ##########################################################

usage(traps(ch[[1]])) <- usage_year1
usage(traps(ch[[2]])) <- usage_year2

#model failed w/ usage info --> most likely due to constant usage in session 2
