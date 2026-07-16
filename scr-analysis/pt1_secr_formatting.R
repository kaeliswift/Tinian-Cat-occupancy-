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

#Removing None & unidentifiable detection & separating | detections
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
  mutate(
    Deployment = mdy_hms(paste(Deployment, "00:00:00"), tz = "Pacific/Guam"),
    Termination = mdy_hms(paste(Termination, "23:59:59"), tz = "Pacific/Guam"),
    Session = ifelse(year(Deployment) == 2024, 1, 2)
  )

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
  mutate(
    Session = ifelse(year(DateTime) == 2024, 1, 2),
    Animal = Individuals 
  )

#Filters to detection > 30 min apart (clusters are 30 min apart)................
data <- data %>%
  arrange(Cluster.ID, Individuals, DateTime) %>%
  group_by(Site.Name, Cluster.ID, Individuals) %>%
  slice(1) %>%
  ungroup()

# 5. Session-wide Occasions ####################################################
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

# creating separate trapfiles per session
trapIDs_year1 <- start_end %>% filter(Session == 1) %>% pull(Site.Name)
trapIDs_year2 <- start_end %>% filter(Session == 2) %>% pull(Site.Name)

traps_year1 <- traps.data %>% filter(TrapID %in% trapIDs_year1) %>% select(TrapID, x, y)
traps_year2 <- traps.data %>% filter(TrapID %in% trapIDs_year2) %>% select(TrapID, x, y)

#traps combined (no session)
traps_combined <- traps.data %>% select(TrapID, x, y)

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

# 8. Create txt Files for secr ################################################
# DROPPING SESSION TO COMBINE ########################################
capt <- data %>% select(Animal, Occasion, TrapID,
                        #Session
                        )

capt_count <- data %>%
  group_by(Individuals, Occasion, TrapID) %>%
  summarise(
    count = n(),
    .groups = "drop"
  ) %>%
  mutate(
    Session = 3   # or whatever session numbers you use
  ) %>%
  select(Session, Individuals, Occasion, TrapID, count)

head(capt_count)

# all capture history data
write.table(capt_count, "capt_count.txt",
            row.names = FALSE, col.names = FALSE,
            quote = FALSE, sep = "\t")

# trap info 
write.table(traps_combined, "traps_combined.txt",
            row.names = FALSE, col.names = FALSE,
            quote = FALSE, sep = "\t")



# 9. Create Capture History ####################################################
ch <- read.capthist(
  captfile = "capt_count.txt",
  trapfile = "traps_combined.txt",
  detector = "count",
  fmt = "trapID"
) # should say: no errors found :-)




# Inspect new ch ###################################################################
summary(ch)
plot(ch, tracks = TRUE)

usage(traps(ch[[1]]))[1:42, ] #currently no usage -> ran into errors due to full usage in session 2 & partial usage in session 1
usage(traps(ch[[2]]))[1:8, ]                      # session 1 is still mainly covered so will ignore usage for now 


# SKIP BELLOW UNLESS YOU NEED TO INSPECT BUFFER SIZE OR EFFORT ##################

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

cats.mask <- secr.fit(ch, mask=masklist, trace = FALSE)

#plot buffer width (m) x n/esa(buffer)ha
par(pty = "s",mar = c(4,4,2,2),mgp =c(2.5,0.8,0),las =1)
esaPlot(cats.HN7000,ylim =c(0,4))
abline(v=7000, col ="red",lty =2)

suggest.buffer(cats.HN7000) #both ~11000 m -> weird
suggest.buffer(cats.mask) #8000 and 9000 m -> not as bad


# Checking D estimates & changes in buffer size #################################
#OK... next moves before models......
#graph buffer sizes & D estimates
#decide on final buffer

#Exploring buffer sizes
buffers <- c(3000, 5000, 7000, 9000, 11000) #checking 3 km to 11 km 

#read in shapefile data
# you will have to download and direct R to your GIS layers (they are too big to store in the repo)

tinian <- st_read(
  "C:/Users/celin/OneDrive/Desktop/Tinian_GIS_layers/CNMI Hi-Res veg data/tinian_release.shp"
)

# make sure CRS matches traps
st_crs(tinian)

# dissolve polygons into single island boundary
island_boundary <- st_union(tinian)
plot(island_boundary)

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

# Inspect ch of cats ###################################################################
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


# Build Usage/Effort Matrices ##################################################

usage_year1 <- make_usage_matrix(1, traps_year1, start_end)
usage_year2 <- make_usage_matrix(2, traps_year2, start_end)


# Attach Usage/Effort ##########################################################

usage(traps(ch[[1]])) <- usage_year1
usage(traps(ch[[2]])) <- usage_year2

#model failed w/ usage info --> most likely due to constant usage in session 2