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

usage(traps(ch[[1]]))[1:42, ] #usage worked
usage(traps(ch[[2]]))[1:8, ] 

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

suggest.buffer(cats.HN7000) #both ~11000 m

#run the null model with a half normal detection
cats.HN<-secr.fit(ch, buffer=8000, trace = FALSE, CL = TRUE)

predict(cats.HN)

esaPlot(cats.HN)

# Set Mask #####################################################################
tinian <- st_read("C:/Users/celin/Tinian-Cat-occupancy-/scr-analysis/CNMI Hi-Res veg data/tinian_release.shp") 
names(tinian)
str(tinian)
plot(tinian["CLASS"]) #what we want the map to look like eventually/where the CLASS covariates come from

#trial based on secr instructions
clippedmask <- make.mask(traps(ch), type = 'trapbuffer', buffer = 8000,
                         poly = tinian)

par(mfrow = c(1,1), mar = c(1,1,1,1))
plot(clippedmask, border = 100, ppoly = FALSE)
polygon(tinian, col = 'lightgreen', border = NA) #fails 
plot(clippedmask, dots = FALSE, mesh = grey(0.4), col = NA, polycol = 'blue', add = TRUE) #looks weird w/ lots of lines
plot(traps(ch[[1]]), pch = 16, cex = 0.8, add = TRUE, col = "red")
plot(traps(ch[[2]]), pch = 16, cex = 0.8, add = TRUE, col = "pink")

#ok use tinian data to extract just island boundary
island_boundary <- st_geometry(st_union(tinian))
plot(island_boundary)
str(island_boundary)

##trial based on secr instructions.... just using island_boundary
clippedmask <- make.mask(traps(ch), type = 'trapbuffer', buffer = 8000,
                         poly = island_boundary)

par(mfrow = c(1,1), mar = c(1,1,1,1))
plot(clippedmask, border = 100, ppoly = FALSE)
plot(clippedmask, dots = FALSE, mesh = grey(0.4), col = NA, polycol = 'blue', add = TRUE) 
plot(traps(ch[1]), pch = 16, cex = 0.8, add = TRUE, col = "red")
plot(traps(ch[[2]]), pch = 16, cex = 0.8, add = TRUE, col = "green") #doesn't work but somehow plots with ch[[1]]

# Adding Covariates to mask ####################################################
# distance to shore............
# need to assign covariates per session & then rejoin mask (otherwise a mismatch of points)
mask1 <- clippedmask[[1]]
mask2 <- clippedmask[[2]]

covariates(mask1) <- data.frame(
  d.to.shore = distancetotrap(mask1, island_boundary))

covariates(mask2) <- data.frame(
  d.to.shore = distancetotrap(mask2, island_boundary))

clippedmask <- list(mask1, mask2)
class(clippedmask) <- "mask"

is.na(covariates(clippedmask)) #good no NAs

str(clippedmask) #looks good

# restructuring habitat class data..........
plot(tinian["CLASS"])
names(tinian)
str(tinian)
table(tinian["CLASS"])

unique(tinian$CLASS) #current available landcover types in tinian shapefile data
unique(traps.data$CLASS.landcover) #landcover types at camera traps themselves

# 1. reclassify
tinian <- tinian %>%
  mutate(Habitat = case_when(
    grepl("Leucaena", CLASS) ~ "tangantangan",
    grepl("Mixed", CLASS) ~ "mixed_introduced",
    grepl("Casuarina", CLASS) ~ "ironwood",
    grepl("Other", CLASS) ~ "shrub_grass",
    grepl("Native", CLASS) ~ "native_limestone",
    TRUE ~ NA_character_
  ))

unique(tinian$CLASS)
unique(tinian$Habitat) #reclassified & now has NAs

par(mfrow = c(1,1), mar = c(1,1,1,1))
plot(tinian["Habitat"])

# 2. rasterize tinian data
tinian_v <- vect(tinian)

r <- rast(tinian_v, resolution = 100, #100 m pixels <- can change this 
          crs = crs(tinian_v))

# rasterize habitat -> need to convert to numeric value
tinian_v$Habitat_ID <- as.factor(tinian_v$Habitat)

habitat_raster <- rasterize(
  tinian_v,
  r,
  field = "Habitat_ID",
  fun = "min"
)


# 3. extract mask points from raster data per session
for (i in 1:2) {
  m <- clippedmask[[i]]
  xy <- data.frame(x = m$x, y = m$y)
  
  covariates(m)$habitat <- terra::extract(
    habitat_raster,
    xy
  )[,2]
  
  clippedmask[[i]] <- m
}

# 4. check that mask values attached
summary(clippedmask)
table(covariates(clippedmask[[1]])$habitat, useNA="ifany")
table(covariates(clippedmask[[2]])$habitat, useNA="ifany")

# 5. address Nas by using nearest raster cell
habitat_fill <- terra::focal(
  habitat_raster,
  w = 3,
  fun = modal,
  na.policy = "only",
  na.rm = TRUE
)

for(i in 1:2){
  
  m <- clippedmask[[i]]
  
  xy <- cbind(m$x, m$y)
  
  ex <- terra::extract(habitat_fill, xy)
  
  covariates(m)$habitat <- ex$Habitat_ID
  
  clippedmask[[i]] <- m
}

table(covariates(clippedmask[[1]])$habitat,
      useNA = "ifany") #check for NAs

table(covariates(clippedmask[[2]])$habitat,
      useNA = "ifany") #check for NAs

levels(tinian_v$Habitat_ID)

# 6. convert raster levels to habitat types
hab_levels <- levels(tinian_v$Habitat_ID)

for(i in 1:2){
  
  m <- clippedmask[[i]]
  
  # numeric raster codes
  h <- covariates(m)$habitat
  
  # replace 0 with NA
  h[h == 0] <- NA
  
  # convert numeric codes to labels
  h_char <- hab_levels[h]
  
  covariates(m)$habitat <- factor(h_char)
  
  clippedmask[[i]] <- m
}

#session 1 
table(covariates(clippedmask[[1]])$habitat,
      useNA = "ifany") #NAs still remaining

#session 2 
table(covariates(clippedmask[[2]])$habitat,
      useNA = "ifany") #NAs still remaining

# 7. copy nearest for remaining NAs
copynearest <- function(mask, covname){
  
  vals <- covariates(mask)[, covname]
  
  na_idx <- is.na(vals)
  
  # mask points with known habitat
  known <- subset(mask, !na_idx)
  
  # nearest known mask point for every point
  nn <- nearesttrap(mask, known)
  
  vals[na_idx] <- covariates(known)[nn[na_idx], covname]
  
  covariates(mask)[, covname] <- vals
  
  return(mask)
}

clippedmask[[1]] <- copynearest(clippedmask[[1]], "habitat")
clippedmask[[2]] <- copynearest(clippedmask[[2]], "habitat")

table(covariates(clippedmask[[1]])$habitat,
      useNA = "ifany") #good no more nas

table(covariates(clippedmask[[2]])$habitat,
      useNA = "ifany") #good no more nas

# 8. set the reference category before modeling 
for(i in 1:2){
  
  covariates(clippedmask[[i]])$habitat <-
    relevel(
      covariates(clippedmask[[i]])$habitat,
      ref = "native_limestone"
    )
}

# 9. check that mask has covariates
names(covariates(clippedmask[[1]]))
names(covariates(clippedmask[[2]]))

summary(clippedmask[[1]])
summary(clippedmask[[2]])

nrow(covariates(clippedmask[[1]]))
length(clippedmask[[1]])

plot(clippedmask[[1]])
plot(st_geometry(tinian), add=TRUE)

# 10. check out map
plot(clippedmask[[1]],
     covariate = "habitat",
     pch = 15,
     cex = 1.2)
plot(traps(ch[[1]]),
     add = TRUE,
     pch = 16)

par(mfrow = c(1,1), mar = c(1,1,1,1))
plot(clippedmask, border = 100, ppoly = FALSE)
plot(clippedmask, dots = FALSE, mesh = grey(0.4), col = NA, polycol = 'blue', add = TRUE) 
plot(traps(ch[1]), pch = 16, cex = 0.8, add = TRUE, col = "red")
plot(traps(ch[[2]]), pch = 16, cex = 0.8, add = TRUE, col = "green") #doesn't work but somehow plots with ch[[1]]

plot(clippedmask[[1]], ppoly = TRUE)
plot(st_geometry(tinian), add = TRUE, border = "grey")
plot(traps(ch[[1]]), add = TRUE, pch = 16)

plot(clippedmask, border = 100)
plot(st_geometry(tinian), add = TRUE, col = NA, border = "grey")
plot(traps(ch[[1]]), add = TRUE, pch = 16)

range(traps(ch[[1]])$x)
range(clippedmask[[1]]$x)

range(traps(ch[[1]])$y)
range(clippedmask[[1]]$y)

plot(clippedmask[[1]], border = 100, ppoly = TRUE)
plot(st_geometry(tinian), add = TRUE, border = "grey")
plot(traps(ch[[1]]), add = TRUE, pch = 16)

#graphing try 2
library(sf)

mask_sf <- st_as_sf(
  data.frame(
    x = clippedmask$x,
    y = clippedmask$y,
    habitat = covariates(clippedmask[[1]])$habitat
  ),
  coords = c("x","y"),
  crs = st_crs(tinian)
)

plot(st_geometry(tinian), col = NA, border = "grey")
plot(mask_sf["habitat"], add = TRUE, pch = 15)
plot(traps(ch[[1]]), add = TRUE, pch = 16)

plot(mask_sf)

#try 3
mask_sf <- st_as_sf(
  data.frame(
    x = clippedmask[[1]]$x,
    y = clippedmask[[1]]$y,
    habitat = covariates(clippedmask[[1]])$habitat
  ),
  coords = c("x","y"),
  crs = st_crs(tinian)
)

# set plot window to MASK extent only
plot(st_geometry(mask_sf), col = NA, axes = TRUE)

# now add layers (they won't reset extent)
plot(st_geometry(tinian), add = TRUE, border = "grey")

plot(mask_sf["habitat"], add = TRUE, pch = 15)

plot(traps(ch[[1]]), add = TRUE, pch = 16)

#try 4: session 1 
plot(clippedmask, border = 100, ppoly = FALSE)
plot(clippedmask[[1]],
     covariate = "habitat",
     pch = 15,
     cex = 1.2,
     add = TRUE)
plot(traps(ch[[1]]), add = TRUE, pch = 16)

#session 2 
plot(clippedmask, border = 100, ppoly = FALSE)
plot(clippedmask[[2]],
     covariate = "habitat",
     pch = 15,
     cex = 1.2,
     add = TRUE)
plot(traps(ch[[2]]), add = TRUE, pch = 16)


# MAY NEED TO REVISIT GRAPHING BUFFER V DENSITY ################################


# Exploring modeling ###########################################################
m0 <- secr.fit(
  ch,
  mask = clippedmask,
  model = list(D ~ 1, g0 ~ 1, sigma ~ 1),
  detectfn = "halfnormal",
  trace = FALSE
) #error in mask structure

class(clippedmask)
str(clippedmask)

summary(clippedmask)

head(clippedmask[[1]]$x)
head(clippedmask[[1]]$y)
length(clippedmask[[1]]$x)
length(clippedmask[[1]]$y)

verify(clippedmask) #error
verify(ch) #no errors

verify(clippedmask[1])
#explore distance to "Urban x" CLASS......................


#trial of clean restart.............
# rebuild masks cleanly
clippedmask <- list(
  make.mask(traps(ch[[1]]), type="trapbuffer", buffer=8000, poly=island_boundary),
  make.mask(traps(ch[[2]]), type="trapbuffer", buffer=8000, poly=island_boundary)
)

# re-add covariates AFTER rebuilding
for(i in 1:2){
  m <- clippedmask[[i]]
  
  xy <- cbind(m$x, m$y)
  
  covariates(m)$habitat <- terra::extract(habitat_fill, xy)$Habitat_ID
  
  clippedmask[[i]] <- m
}

# verify individually
verify(clippedmask[[1]])
verify(clippedmask[[2]])
verify(clippedmask)

class(clippedmask)

class(clippedmask) <- "mask"
attr(clippedmask, "type") <- "trapbuffer"

#trial of defining session masks as lists
masklist <- list(`1` = clippedmask[[1]],
                 `2` = clippedmask[[2]])

class(masklist)
verify(masklist) #nope

anyNA(masklist[[1]]$x)
anyNA(masklist[[1]]$y)
anyNA(masklist[[2]]$x)
anyNA(masklist[[2]]$y)

#try again..... worked :)
m1 <- clippedmask[[1]]
m2 <- clippedmask[[2]]

attr(m1, "type") <- "trapbuffer"
attr(m2, "type") <- "trapbuffer"

masklist <- list(`1` = m1, `2` = m2)

class(masklist) <- c("mask", "list")   # sometimes required

# then fit
m0 <- secr.fit(ch, mask = masklist,
               model = list(D~1, g0~1, sigma~1),
               detectfn="halfnormal")

m0

names(covariates(masklist)) #NOPE ---> BIG ISSUE.............................

m0 <- secr.fit(ch, mask = masklist,
               model = list(D~Habitat_ID, g0~1, sigma~1),
               detectfn="halfnormal")


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
