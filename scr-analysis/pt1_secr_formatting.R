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

# creating separate trapfiles per session
trapIDs_year1 <- start_end %>% filter(Session == 1) %>% pull(Site.Name)
trapIDs_year2 <- start_end %>% filter(Session == 2) %>% pull(Site.Name)

traps_year1 <- traps.data %>% filter(TrapID %in% trapIDs_year1) %>% select(TrapID, x, y)
traps_year2 <- traps.data %>% filter(TrapID %in% trapIDs_year2) %>% select(TrapID, x, y)

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
usage_year1 <- make_usage_matrix(traps_year1, start_end)
usage_year1

usage_year2 <- make_usage_matrix(traps_year2, start_end)
usage_year2


# 9. Create txt Files for secr ################################################
capt <- data %>%
  group_by(Individuals, Session, Occasion, TrapID) %>%
  summarise(Count = n(),
    .groups = "drop") %>%
  rename(Animal = Individuals) %>% 
  select(Session, Animal, Occasion, TrapID, Count)

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

# 10. Create Capture History ####################################################
ch <- read.capthist(
  captfile = "capt_all.txt",
  trapfile = list("traps_year1.txt", "traps_year2.txt"),
  detector = "count",
  fmt = "trapID"
) # should say: no errors found :-)


# Attach Usage/Effort 
usage(traps(ch[[1]])) <- usage_year1
usage(traps(ch[[2]])) <- usage_year2

# 11. Inspect new ch ###################################################################
summary(ch)
traps(ch)
plot(ch, tracks = TRUE)

saveRDS(ch, "m_ch.rds")
ch <- readRDS("m_ch.rds")
verify(ch)


# SKIP BELLOW UNLESS YOU NEED TO INSPECT BUFFER SIZE OR EFFORT ##################

# Rough estimate of buffer size ################################################
#estimate of sigma HN to suggest buffer size
#buffer size is usually 4 sigma HN
RPSV(ch, CC = TRUE)
#session 1 = 1922.353 * 4 = 7689.412
#session 2 = 756.6633 * 4 = 3026.653

# different estimate of buffer for HN
suggest.buffer(ch, detectfn = 'HN', RBtarget = 0.001)
# session 1 = 8410
# session 2 = 3390

#run the null model with a half normal detection
cats.HN7000 <-secr.fit(ch, buffer=7000, trace = FALSE)

cats.HN7000 

cats.HN8000 <-secr.fit(ch, buffer=8000, trace = FALSE)

cats.HN8000

#plot buffer width (m) x n/esa(buffer)ha
par(pty = "s",mar = c(4,4,2,2),mgp =c(2.5,0.8,0),las =1)
esaPlot(cats.HN7000,ylim =c(0,4))
abline(v=7000, col ="red",lty =2)

suggest.buffer(cats.HN7000) #both ~12000 m -> weird
suggest.buffer(cats.HN8000) #both ~12000 m -> weird

# Checking D estimates & changes in buffer size #################################
#OK... next moves before models......
#graph buffer sizes & D estimates
#decide on final buffer

#Exploring buffer sizes
buffers <- c(3000, 5000, 7000, 9000, 11000, 13000) #checking 3 km to 13 km 

#read in shapefile data
# you will have to download and direct R to your GIS layers (they are too big to store in the repo)

tinian <- st_read(
  "C:/Users/celin/OneDrive/Desktop/Tinian_GIS_layers/CNMI Hi-Res veg data/tinian_release.shp"
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


#checking if D estimate stabilizes by looking at values
D_df #8000 m looks ok

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

