# Formatting of TrapTagger data for secr analysis .........................

# load needed packages
# use install.packages("package") if not installed yet
library(secr)
library(tidyverse)
library(lubridate)
library(sf)
library(sp)
library(ggplot2)
library(readr)
library(terra)

# 1. Load TrapTagger Data #############################################
raw.data <- read.csv("TrapTagger_Cat_Individuals.csv")

#Removing None/unidentifiable  detection & separating | detections & setting time zone
data <- raw.data %>%
  filter(Individuals != "None") %>%
  mutate(Individuals = str_trim(Individuals)) %>%
  separate_rows(Individuals, sep = "\\|") %>%
  filter(Individuals != "unidentifiable") %>% 
  mutate(DateTime = mdy_hm(Timestamp, tz = "Pacific/Guam"))

# 2. Load Deployment Data ############################################
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
  mutate(Deployment = mdy_hms(paste(Deployment, "00:00:00"), tz = "Pacific/Guam"),
    Termination = mdy_hms(paste(Termination, "23:59:59"), tz = "Pacific/Guam"),
    Session = ifelse(year(Deployment) == 2024, 1, 2))

# 3. Filter cat data to within study period ###################################
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

# 4. Define Session & Clean detections to > 30 min apart #######################
data <- data %>%
  mutate(Session = ifelse(year(DateTime) == 2024, 1, 2),
    Animal = Individuals)

#Filters to detection > 30 min apart (clusters are 30 min apart)................
data <- data %>%
  arrange(Cluster.ID, Individuals, DateTime) %>%
  group_by(Site.Name, Cluster.ID, Individuals) %>%
  slice(1) %>%
  ungroup()

# 5. Session-wide Occasions ####################################################
# occasion start dates are camera deployment dates
data <- data %>%
  mutate(Occasion = floor(as.numeric(difftime(DateTime, Deployment, units = "days")) / 7) + 1) 

# filter to 1-6 occasions
table(data$Occasion)
data <- data %>% filter(Occasion >= 1 & Occasion <= 6)

# 6. Define TrapID ##########################################################
data <- data %>%
  mutate(TrapID = Site.Name)


# 7. Format Trap Data ########################################################
traps.data <- read.csv("cat_cam_deployment_landcover_type.csv") %>%
  rename(TrapID = Label, x = Longitude, y = Latitude) %>%
  mutate(TrapID = str_trim(TrapID))

# transforming x,y coordinates to secr format 
traps.sf <- st_as_sf(traps.data, coords = c("x", "y"), crs = 4326) %>%
  st_transform(crs = 32655)

coords <- st_coordinates(traps.sf)

traps.data <- traps.data %>%
  mutate(x = coords[,1], y = coords[,2])

#traps combined (no sessions)
traps_combined <- traps.data %>% select(TrapID, x, y)

# 8. Attach effort #############################################################
# Create effort matrix function 
make_usage_matrix <- function(traps_df, deploy_df) {
  
  trap_ids <- traps_df$TrapID
  deploy_sess <- deploy_df 
  
  usage_mat <- matrix(
    0,
    nrow = length(trap_ids),
    ncol = 6,
    dimnames = list(trap_ids, paste0("occ", 1:6))
  )
  
  for(i in seq_along(trap_ids)) {
    
    trap_info <- deploy_sess %>%
      filter(Site.Name == trap_ids[i])
    
    if(nrow(trap_info) == 0) next
    
    # First deployment for this trap
    trap_start <- min(trap_info$Deployment)
    
    for(k in 1:6) {
      
      occ_start <- trap_start + days((k - 1) * 7)
      occ_end   <- occ_start + days(7) - seconds(1)
      
      total_overlap <- 0
      
      for(j in 1:nrow(trap_info)) {
        
        overlap_start <- max(trap_info$Deployment[j], occ_start)
        overlap_end   <- min(trap_info$Termination[j], occ_end)
        
        if(overlap_end > overlap_start) {
          total_overlap <- total_overlap +
            as.numeric(difftime(overlap_end, overlap_start, units = "days"))
        }
      }
      
      usage_mat[i, k] <- round(min(1, total_overlap / 7), 3)
    }
  }
  
  usage_mat
}

# Build Usage/Effort Matrices 
usage_combined <- make_usage_matrix(traps_combined, start_end)

usage_combined


# INSPECT # of cats in both sessions ########################################
cats_both <- data %>%
  distinct(Individuals, Session) %>%   # one row per cat per session
  count(Individuals) %>%               # number of sessions each cat appears in
  filter(n > 1)

cats_both

cats_both_ids <- cats_both$Individuals

# Join trap coordinates onto every detection
detections <- data %>%
  left_join(traps.data, by = c("TrapID"))

# calculate the shift in activity "center" in m
centers <- detections %>%
  filter(Individuals %in% cats_both_ids) %>%
  group_by(Individuals, Session) %>%
  summarize(
    meanX = mean(as.numeric(x)),
    meanY = mean(as.numeric(y)),
    nDetections = n(),
    .groups = "drop"
  )

centers_mean <- centers %>%
  select(-nDetections) %>%   # remove session-specific count
  pivot_wider(
    names_from = Session,
    values_from = c(meanX, meanY)
  ) %>%
  mutate(
    shift_m = sqrt((meanX_2 - meanX_1)^2 +
                     (meanY_2 - meanY_1)^2)
  )

centers_mean

# find closest distance btw cameras btw sessions
cat_both_cameras <- detections %>%
  filter(Individuals %in% cats_both_ids) %>%
  distinct(Individuals, Session, TrapID, x, y)

min_dist_pair <- cat_both_cameras %>%
  group_by(Individuals) %>%
  group_modify(~{
    
    s1 <- distinct(filter(.x, Session == 1), TrapID, x, y)
    s2 <- distinct(filter(.x, Session == 2), TrapID, x, y)
    
    if (nrow(s1) == 0 || nrow(s2) == 0) {
      return(tibble(
        min_distance_m = NA_real_,
        trap_s1 = NA_character_,
        trap_s2 = NA_character_
      ))
    }
    
    # calculate only cross-session distances
    cross <- outer(
      1:nrow(s1),
      1:nrow(s2),
      Vectorize(function(i, j) {
        sqrt((s1$x[i] - s2$x[j])^2 +
               (s1$y[i] - s2$y[j])^2)
      })
    )
    
    min_index <- which(cross == min(cross), arr.ind = TRUE)
    
    tibble(
      min_distance_m = min(cross),
      trap_s1 = s1$TrapID[min_index[1, 1]],
      trap_s2 = s2$TrapID[min_index[1, 2]]
    )
    
  }) %>%
  ungroup()

min_dist_pair

# checking max distance btw cameras btw sessions
home_range_span <- cat_both_cameras %>%
  group_by(Individuals, Session) %>%
  group_modify(~{
    coords <- distinct(.x, TrapID, x, y)
    
    if(nrow(coords) < 2){
      return(tibble(max_camera_distance_m = 0))
    }
    
    tibble(
      max_camera_distance_m = max(as.matrix(dist(coords[,c("x","y")])))
    )
  })

home_range_span

# how many cameras the cats showed up on
camera_summary <- cat_both_cameras %>%
  group_by(Individuals, Session) %>%
  summarize(
    n_cameras = n_distinct(TrapID),
    cameras = paste(sort(unique(TrapID)), collapse = ", "),
    .groups = "drop"
  )

camera_summary

# 9. Create txt Files for secr ################################################
# DROPPING SESSIONS TO COMBINE 
capt <- data %>% select(Animal, Occasion, TrapID,
                        #Session
                        )

capt_count <- data %>%
  group_by(Individuals, Occasion, TrapID) %>%
  summarise(
    Count = n(),
    .groups = "drop"
  ) %>%
  mutate(
    Session = 3   # combines Session 1 & 2 (Session 3 is imaginary/the combined of 1 & 2)
  ) %>%
  select(Session, Individuals, Occasion, TrapID, Count)

head(capt_count)

# all capture history data
write.table(capt_count, "capt_count.txt",
            row.names = FALSE, col.names = FALSE,
            quote = FALSE, sep = "\t")

# trap info 
write.table(traps_combined, "traps_combined.txt",
            row.names = FALSE, col.names = FALSE,
            quote = FALSE, sep = "\t")



# 10. Create Capture History ####################################################
ch <- read.capthist(
  captfile = "capt_count.txt",
  trapfile = "traps_combined.txt",
  detector = "count",
  fmt = "trapID"
) # should say: no errors found :-)

# Attach Usage/Effort 
usage(traps(ch)) <- usage_combined

# 11. Add Season covariate at trap-level #######################################
# Add season covariate to each trap
trap_season <- start_end %>%
  group_by(Site.Name) %>%
  summarise(
    season = first(Session),   # each trap only belongs to one session
    .groups = "drop"
  ) %>%
  rename(TrapID = Site.Name)

traps_combined <- traps_combined %>%
  left_join(trap_season, by = "TrapID") %>%
  mutate(
    season = factor(season,
                    levels = c(1, 2),
                    labels = c("fall", "spring"))
  )

# attach trap covariates
covariates(traps(ch)) <- traps_combined %>%
  select(season)

covariates(traps(ch))
table(covariates(traps(ch))$season)

# 12. Inspect new ch ###################################################################
summary(ch)
traps(ch)
plot(ch, tracks = TRUE)

saveRDS(ch, "ch.rds")

# SKIP BELLOW UNLESS YOU NEED TO INSPECT BUFFER SIZE OR EFFORT ##################

# Rough estimate of buffer size ################################################
#estimate of sigma HN to suggest buffer size
#buffer size is usually 4 sigma HN
RPSV(ch, CC = TRUE)
# 1867.963 * 4 = 7471.852

# different estimate of buffer for HN
suggest.buffer(ch, detectfn = 'HN', RBtarget = 0.001)
# 8170

#run the null model with a half normal detection
cats.HN7000 <-secr.fit(ch, buffer=7000, trace = FALSE)

cats.HN7000 

cats.HN8000 <-secr.fit(ch, buffer=8000, trace = FALSE)

cats.HN8000

#plot buffer width (m) x n/esa(buffer)ha
par(pty = "s",mar = c(4,4,2,2),mgp =c(2.5,0.8,0),las =1)
esaPlot(cats.HN7000,ylim =c(0,4))
abline(v=7000, col ="red",lty =2)

suggest.buffer(cats.HN7000) #both ~11000 m -> weird
suggest.buffer(cats.HN8000) #8000 and 9000 m -> not as bad


# Checking D estimates & changes in buffer size #################################
#OK... next moves before models......
#graph buffer sizes & D estimates
#decide on final buffer

#Exploring buffer sizes
buffers <- c(3000, 5000, 7000, 9000, 11000, 13000) #checking 3 km to 11 km 

#read in shapefile data
# you will have to download and direct R to your GIS layers (they are too big to store in the repo)

tinian <- st_read(
  "C:/Users/celin/OneDrive/Desktop/Tinian_GIS_layers/CNMI Hi-Res veg data/tinian_release.shp")

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
  derived(fit)["D", "estimate"]
})

#plot estimates vs buffer size
D_df <- data.frame(
  buffer = buffers,
  D = D_values
)

D_df

plot(
  D_df$buffer,
  D_df$D,
  type = "b",
  xlab = "Buffer distance (m)",
  ylab = "Density (cats/ha)"
)


#checking if D estimate stabilizes by looking at values
D_df

#check in N stabilizes 
region.N(fits[[which(buffers == 7000)]])
region.N(fits[[which(buffers == 9000)]])
region.N(fits[[which(buffers == 11000)]])

# Ok trying again but including 8000 m..............
#Exploring buffer sizes
buffers <- c(7000, 8000, 9000, 10000, 11000) #checking 7 km to 11 km 

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
  derived(fit)["D", "estimate"]
})

#plot estimates vs buffer size
D_df <- data.frame(
  buffer = buffers,
  D = D_values
)

D_df

plot(
  D_df$buffer,
  D_df$D,
  type = "b",
  xlab = "Buffer distance (m)",
  ylab = "Density (cats/ha)"
)


#checking if D estimate stabilizes by looking at values
D_df

#check in N stabilizes 
region.N(fits[[which(buffers == 7000)]])
region.N(fits[[which(buffers == 8000)]])
region.N(fits[[which(buffers == 9000)]])
region.N(fits[[which(buffers == 10000)]])
region.N(fits[[which(buffers == 11000)]])

#Choosing Buffer = 8000 for now!!!!!!!!
suggest.buffer(fits[[2]])
esaPlot(fits[[2]])

# Inspect ch of individual cats ###################################################################
summary(ch)
plot(ch, tracks = TRUE)

usage(traps(ch[[1]]))[1:42, ] #currently no usage -> ran into errors due to full usage in session 2& partial usage in session 1
usage(traps(ch[[2]]))[1:8, ]                      # session 1 is still mainly covered so will ignore usage for now 

# ch for session 1
plot(island_poly)
plot(ch[[1]], tracks = TRUE, add = TRUE)

#ch for session 2
plot(island_poly)
plot(ch[[2]], tracks = TRUE, add = TRUE)



# individuals for session 1...........
ids <- rownames(ch[[1]])

par(mfrow = c(2,3))

for (id in ids) {
  
  ch.ind <- subset(ch[[1]], subset = id)
  
  plot(island_poly, axes = FALSE)
  
  suppressWarnings(
    plot(ch.ind,
         tracks = TRUE,
         add = TRUE)
  )
  
  mtext(id, side = 2, line = 0.2, cex = 0.8)
}

# generate pdf 
pdf("individual_tracks_session1.pdf",
    width = 11,
    height = 8.5)

ids <- rownames(ch[[1]])

par(mfrow = c(2,3), mar = c(1,1,2,1))

for (id in ids) {
  
  ch.ind <- subset(ch[[1]], subset = id)
  
  plot(island_poly, axes = FALSE)
  
  suppressWarnings(
    plot(ch.ind,
         tracks = TRUE,
         add = TRUE)
  )
  
  mtext(id, side = 2, line = 0.2, cex = 0.8)
}

dev.off()

# individuals for session 2 ...........
# generate pdf 
pdf("individual_tracks_session2.pdf",
    width = 11,
    height = 8.5)

ids <- rownames(ch[[2]])

par(mfrow = c(2,3), mar = c(1,1,2,1))

for (id in ids) {
  
  ch.ind <- subset(ch[[2]], subset = id)
  
  plot(island_poly, axes = FALSE)
  
  suppressWarnings(
    plot(ch.ind,
         tracks = TRUE,
         add = TRUE)
  )
  
  mtext(id, side = 2, line = 0.2, cex = 0.8)
}

dev.off()

# detections session 1
detections <- apply(ch[[1]], 1, sum)

det.table <- data.frame(
  animalID = names(detections),
  detections = detections
)

det.table

occasions.detected <- apply(
  ch[[1]],
  1,
  function(x) sum(apply(x, 1, sum) > 0)
)

data.frame(
  animalID = rownames(ch[[1]]),
  totalDetections = apply(ch[[1]], 1, sum),
  occasionsDetected = occasions.detected
)

trap.visits <- apply(
  ch[[1]],
  1,
  function(x) sum(colSums(x) > 0)
)

summary.table.sess1 <- data.frame(
  animalID = rownames(ch[[1]]),
  totalDetections = apply(ch[[1]], 1, sum),
  occasionsDetected = occasions.detected,
  trapsVisited = trap.visits
)

summary.table.sess1 <- summary.table.sess1[order(summary.table.sess1$totalDetections,
                                                 decreasing = TRUE), ]

summary.table.sess1

write.csv(summary.table.sess1, "session1_detections.csv")

# detections session 2
detections <- apply(ch[[2]], 1, sum)

det.table <- data.frame(
  animalID = names(detections),
  detections = detections
)

det.table

occasions.detected <- apply(
  ch[[2]],
  1,
  function(x) sum(apply(x, 1, sum) > 0)
)

data.frame(
  animalID = rownames(ch[[2]]),
  totalDetections = apply(ch[[2]], 1, sum),
  occasionsDetected = occasions.detected
)

trap.visits <- apply(
  ch[[2]],
  1,
  function(x) sum(colSums(x) > 0)
)

summary.table.sess2 <- data.frame(
  animalID = rownames(ch[[2]]),
  totalDetections = apply(ch[[2]], 1, sum),
  occasionsDetected = occasions.detected,
  trapsVisited = trap.visits
)

summary.table.sess2 <- summary.table.sess2[order(summary.table.sess2$totalDetections,
                                                 decreasing = TRUE), ]

summary.table.sess2

write.csv(summary.table.sess2, "session2_detections.csv")

