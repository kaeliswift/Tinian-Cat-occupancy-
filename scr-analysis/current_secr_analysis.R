#Cleaning of data & mask formation after Kaeli meeting
#5/29/2026

library(secr)
library(tidyverse)
library(lubridate)
library(sf)
library(sp)
library(ggplot2)
library(readr)
library(terra)

# Format Data for secr #########################################################
#Load TrapTagger Data ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
raw.data <- read.csv("TrapTagger_Cat_Individuals.csv")

#Removing None & unidentifiable detection & separating | detections
data <- raw.data %>%
  filter(Individuals != "None") %>%
  mutate(Individuals = str_trim(Individuals)) %>%
  separate_rows(Individuals, sep = "\\|") %>%
  filter(Individuals != "unidentifiable") %>%
  mutate(DateTime = mdy_hm(Timestamp, tz = "Pacific/Guam"))

#Load Deployment Data ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
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

#Filter cat data to within study period ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
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

#Define Session & Clean detections to > 30 min apart ~~~~~~~~~~~~~~~~~~~~~~~~~~~

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

# Session-wide Occasions ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
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

# Define TrapID ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
data <- data %>%
  mutate(TrapID = Site.Name)

# Format Trap Data ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
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



# Create txt Files ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
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

# Create Capture History ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
ch <- read.capthist(
  captfile = "capt_all.txt",
  trapfile = list("traps_year1.txt", "traps_year2.txt"),
  detector = "count",
  fmt = "trapID"
) # should say: no errors found :-)

# Add Traps covariates ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
traps1 <- traps(ch[[1]])
traps2 <- traps(ch[[2]])

traps_year1 <- traps.data %>% filter(TrapID %in% trapIDs_year1) %>% select(TrapID, x, y, CLASS.landcover)
traps_year2 <- traps.data %>% filter(TrapID %in% trapIDs_year2) %>% select(TrapID, x, y, CLASS.landcover)

# Add CLASS.landcover covariate to Traps (site-specific covariate) .............
covariates(traps1) <- data.frame(site_habitat = traps_year1$CLASS.landcover)
covariates(traps2) <- data.frame(site_habitat = traps_year2$CLASS.landcover)

traps(ch[[1]]) <- traps1
traps(ch[[2]]) <- traps2

table(covariates(traps(ch[[1]]))) #worked
table(covariates(traps(ch[[2]])))


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

cats.mask <- secr.fit(ch, mask=masklist, trace = FALSE)

#plot buffer width (m) x n/esa(buffer)ha
par(pty = "s",mar = c(4,4,2,2),mgp =c(2.5,0.8,0),las =1)
esaPlot(cats.HN7000,ylim =c(0,4))
abline(v=7000, col ="red",lty =2)

suggest.buffer(cats.HN7000) #both ~11000 m -> weird
suggest.buffer(cats.mask) #8000 and 9000 m -> not as bad


#Checking D estimates & changes in buffer size #################################
#OK... next moves before models......
#graph buffer sizes & D estimates
#decide on final buffer

#Exploring buffer sizes
buffers <- c(3000, 5000, 7000, 9000, 11000) #checking 3 km to 11 km 

#read in shapefile data
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


# Set Mask and Analysis ########################################################

# 1. read shapefile................
tinian <- st_read(
  "C:/Users/celin/OneDrive/Desktop/Tinian_GIS_layers/CNMI Hi-Res veg data/tinian_release.shp"
)

# make sure CRS matches traps
st_crs(tinian)

# dissolve polygons into single island boundary
island_boundary <- st_union(tinian)

plot((island_boundary)) #shows island boundary

# create spatial polygon.... more clear for secr
island_sp <- as(
  st_cast(island_boundary, "MULTIPOLYGON"),
  "Spatial"
)

plot(island_sp)

# 2. reclassify habitat...............
tinian <- tinian %>%
  mutate(
    habitat = case_when(
      grepl("Leucaena", CLASS) ~ "tangantangan",
      grepl("Mixed", CLASS) ~ "mixed_introduced",
      grepl("Casuarina", CLASS) ~ "mixed_introduced", #ironwood is included in mixed_introduced
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

plot(habitat_raster) #nice

# 4. build session masks separately...........

# define shoreline
shoreline <- st_boundary(island_boundary)
plot(shoreline)

# define roads 
roads <- st_read(
  "C:/Users/celin/OneDrive/Desktop/Tinian_GIS_layers/Tinian_roads/Roads Tinian.shp"
)
plot(roads["GlobalID"])

roads <- st_geometry(roads)
plot(roads)

# make sure roads has epsg (missing w/o below command)
st_crs(roads) <- st_crs(tinian) 

# define MLA activity areas
MLA_activity <- st_read(
  "C:/Users/celin/OneDrive/Desktop/Tinian_GIS_layers/MLA_activity_areas/MLA_activity areas.shp"
)

plot(st_geometry(island_boundary))
plot(st_geometry(MLA_activity), add = TRUE, col = "red")

MLA_activity <- st_geometry(MLA_activity)

# extract elevation
elev <- rast("C:/Users/celin/OneDrive/Desktop/Tinian_GIS_layers/tinian_dem")
crs(elev)
crs(elev) <- "EPSG:32655"

plot(elev)

# extract slope from elevation
slope <- terrain(
  elev,
  v = "slope",
  unit = "degrees",
  neighbors = 8
)


# extract human areas separately & together
humans <- st_read(
  "C:/Users/celin/OneDrive/Desktop/Tinian_GIS_layers/Human_activity/humans.shp")

plot(st_geometry(island_boundary))
plot(st_geometry(humans), add = TRUE, col = "pink")

table(humans$Name) #available human locations

Quarry <- humans %>% 
  filter(Name == "Quarry") %>% 
  st_geometry()
plot(st_geometry(island_boundary))
plot(st_geometry(Quarry), add = TRUE, col = "red")

NorthField <- humans %>% 
  filter(Name == "North Field") %>% 
  st_geometry()
plot(st_geometry(NorthField), add = TRUE, col = "orange")

Airport <- humans %>% 
  filter(Name == "Airport") %>% 
  st_geometry()
plot(st_geometry(Airport), add = TRUE, col = "yellow")

CampTinian <- humans %>% 
  filter(Name == "Camp Tinian") %>% 
  st_geometry()
plot(st_geometry(CampTinian), add = TRUE, col = "green")

VOA <- humans %>% 
  filter(Name == "Voice of America") %>% 
  st_geometry()
plot(st_geometry(VOA), add = TRUE, col = "blue")

Dump <- humans %>% 
  filter(Name == "Dump") %>% 
  st_geometry()
plot(st_geometry(Dump), add = TRUE, col = "violet")

Town <- humans %>% 
  filter(Name == "Town") %>% 
  st_geometry()
plot(st_geometry(Town), add = TRUE, col = "purple")
 
legend(
  "topleft",
  legend = c(
    "Quarry",
    "North Field",
    "Airport",
    "Camp Tinian",
    "VOA",
    "Dump",
    "Town"
  ),
  fill = c(
    "red",
    "orange",
    "yellow",
    "green",
    "blue",
    "violet",
    "purple"
  ),
  bty = "n")

humans <- st_geometry(humans)

# create masks per session w/ covariates .............................

# convert island_boundary to SpatVect
island_poly <- vect(island_boundary)

# create masks..................................................................
masklist <- lapply(
  1:2,
  function(i){
    
    # -------------------------
    # BUILD TRAPBUFFER MASK
    # -------------------------
    
    m <- make.mask(
      traps(ch[[i]]),
      type = "trapbuffer",
      buffer = 7000,
      spacing = 250, 
      poly = island_poly
    )
    
    # define coordinates
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
        ref = "tangantangan" #level with the most points needs to be reference level
      )
    
    # -------------------------
    # DISTANCE TO SHORE & ROADS
    # -------------------------
    
    # convert mask points to sf
    pts <- st_as_sf(
      data.frame(x = m$x, y = m$y),
      coords = c("x", "y"),
      crs = st_crs(tinian)
    )
    
    # distance to shoreline
    dshore <- st_distance(
      pts,
      shoreline
    )
    
    covariates(m)$d.to.shore <- as.numeric(dshore)
    
    # distance to road
    droads <- st_distance(
      pts,
      roads
    )
    
    covariates(m)$d.to.road <- as.numeric(apply(droads, 1, min)) #select only nearest road
    
    # -------------------------
    # MLA ACTIVITY ZONES -- no buffer around area rn.... may need to change
    # -------------------------
    #inside or outside high activity MLA areas
    inside_MLA <- st_intersects(
      pts,
      MLA_activity,
      sparse = FALSE
    )
    
    covariates(m)$MLA <- as.integer(
      rowSums(inside_MLA) > 0
    )
    
    covariates(m)$MLA <- factor(
      ifelse(rowSums(inside_MLA) > 0,
             "inside",
             "outside")
    )
    
    # distance from high activity MLA areas
    dMLA <- st_distance(
      pts,
      MLA_activity
    )
    
    covariates(m)$d.to.MLA <- as.numeric(apply(dMLA, 1, min)) #select only nearest 
    
    # -------------------------
    # ELEVATION
    # -------------------------
    
    elev_vals <- terra::extract(
      elev,
      xy
    )
    
    covariates(m)$elev <- elev_vals$tinian_dem
    
    # -------------------------
    # SLOPE
    # -------------------------
    slope_vals <- terra::extract(slope, xy)
    
    covariates(m)$slope <- slope_vals[,1]
    
    # -------------------------
    # DISTANCE TO HUMAN ACTIVITY AREAS
    # -------------------------
    dhumans <- st_distance(
      pts,
      humans
    )
    
    covariates(m)$d.to.humans <- as.numeric(apply(dhumans, 1, min))
    
    dAirport <- st_distance(
      pts,
      Airport
    )
    
    covariates(m)$d.to.Airport <- as.numeric(apply(dAirport, 1, min))
    
    dCampTinian <- st_distance(
      pts,
      CampTinian
    )
    
    covariates(m)$d.to.CampTinian <- as.numeric(apply(dCampTinian, 1, min))
    
    dDump <- st_distance(
      pts,
      Dump
    )
    
    covariates(m)$d.to.Dump <- as.numeric(apply(dDump, 1, min))
    
    dNorthField <- st_distance(
      pts,
      NorthField
    )
    
    covariates(m)$d.to.NorthField <- as.numeric(apply(dNorthField, 1, min))
    
    dQuarry <- st_distance(
      pts,
      Quarry
    )
    
    covariates(m)$d.to.Quarry <- as.numeric(apply(dQuarry, 1, min))
    
    dTown <- st_distance(
      pts,
      Town
    )
    
    covariates(m)$d.to.Town <- as.numeric(apply(dTown, 1, min))
    
    dVOA <- st_distance(
      pts,
      VOA
    )
    
    covariates(m)$d.to.VOA <- as.numeric(apply(dVOA, 1, min))
    
    m
  }
)
# ..............................................................................


# check mask points
nrow(masklist[[1]]) #1580
nrow(masklist[[2]]) #945

# check covariates
summary(covariates(masklist[[1]]))
summary(covariates(masklist[[2]]))


# check habitat 
summary(covariates(masklist[[1]])$habitat)
summary(covariates(masklist[[2]])$habitat) 

#will need to reassign NAs

# check d.to.shore
summary(covariates(masklist[[1]])$d.to.shore)
summary(covariates(masklist[[2]])$d.to.shore)

# check d.to.road
summary(covariates(masklist[[1]])$d.to.road)
summary(covariates(masklist[[2]])$d.to.road)

# check MLA activity zones
summary(covariates(masklist[[1]])$MLA)
summary(covariates(masklist[[2]])$MLA)

# check distance to MLA activity zones
summary(covariates(masklist[[1]])$d.to.MLA)
summary(covariates(masklist[[2]])$d.to.MLA)

# summary of each session mask
summary(masklist[[1]])
summary(masklist[[2]])

# check structure of masks
verify(masklist[[1]]) #good
verify(masklist[[2]]) #good
verify(masklist) #issue -> will need to correct

# check all covariates there
names(covariates(masklist[[1]]))
names(covariates(masklist[[2]]))

# check numeric covariate correlation (r > 0.5 not good)
# distance to shore & road correlation check
cor(
  covariates(masklist[[1]])$d.to.shore,
  covariates(masklist[[1]])$d.to.road
) #good

cor(
  covariates(masklist[[2]])$d.to.shore,
  covariates(masklist[[2]])$d.to.road
) #good

# distance to road & MLA activity area correlation check
cor(
  covariates(masklist[[1]])$d.to.road,
  covariates(masklist[[1]])$d.to.MLA
) #good

cor(
  covariates(masklist[[2]])$d.to.road,
  covariates(masklist[[2]])$d.to.MLA
) #good

# distance to shore & MLA activity area correlation check
cor(
  covariates(masklist[[1]])$d.to.shore,
  covariates(masklist[[1]])$d.to.MLA
) #good

cor(
  covariates(masklist[[2]])$d.to.shore,
  covariates(masklist[[2]])$d.to.MLA
) #good

# distance to shore & elevation
cor(
  covariates(masklist[[1]])$d.to.shore,
  covariates(masklist[[1]])$elev
) #ok r = 0.4

cor(
  covariates(masklist[[2]])$d.to.shore,
  covariates(masklist[[2]])$elev
) #not good r = 0.67

# plot each session mask
plot(island_poly)
plot(masklist[[1]], pch = 15, cex = 0.3, add = TRUE)

plot(island_poly)
plot(masklist[[2]], pch = 15, cex = 0.3, add = TRUE)

# double check mask structure
class(masklist[[1]])
class(masklist[[2]])

attributes(masklist[[1]])
attributes(masklist[[2]])

# 5. replace any blanks in habitat with NA............
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


# 6. replace NAs in habitat & 1 in elev with nearest neighbor..........
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

#replace elev NAs with avg elev.....
#session 1 (only 1 NA there)
m <- masklist[[1]]

xy <- cbind(m$x, m$y)

elev_vals <- terra::extract(elev, xy)

idx <- which(is.na(elev_vals$tinian_dem))

elev_vals$tinian_dem[idx] <- median(elev_vals$tinian_dem, na.rm = TRUE)

covariates(m)$elev <- elev_vals$tinian_dem

masklist[[1]] <- m

#check it worked
summary(covariates(masklist[[1]])$elev)
summary(covariates(masklist[[2]])$elev)

#replace slope NAs with avg elev.....
#session 1 
m <- masklist[[1]]

xy <- cbind(m$x, m$y)

slope_vals <- terra::extract(slope, xy)

idx <- which(is.na(slope_vals$slope))

slope_vals$slope[idx] <- median(slope_vals$slope, na.rm = TRUE)

covariates(m)$slope <- slope_vals$slope

masklist[[1]] <- m

#session 2
m <- masklist[[2]]

xy <- cbind(m$x, m$y)

slope_vals <- terra::extract(slope, xy)

idx <- which(is.na(slope_vals$slope))

slope_vals$slope[idx] <- median(slope_vals$slope, na.rm = TRUE)

covariates(m)$slope <- slope_vals$slope

masklist[[2]] <- m

#check it worked
summary(covariates(masklist[[1]])$slope)
summary(covariates(masklist[[2]])$slope)

# 6. force secr to recognize both session masks.............
class(masklist) <- c("mask", "list")
verify(masklist)

#session 1 mask
plot(island_poly)
plot(masklist[[1]], pch = 15, cex = 0.4, add = TRUE)
plot(traps(ch[[1]]), add = TRUE, col = "red", pch = 16)

#session 2 mask
plot(island_poly)
plot(masklist[[2]], pch = 15, cex = 0.4, add = TRUE)
plot(traps(ch[[2]]), add = TRUE, col = "red", pch = 16)

plot(masklist) #weird that session 1 is so different

# 7. graph mask.........................

# habitat data
plot(island_poly)
plot(masklist[[1]], covariate = "habitat", pch = 15, cex = 0.6, add = TRUE)
plot(traps(ch[[1]]), add = TRUE, pch = 16)

plot(island_poly)
plot(masklist[[2]], covariate = "habitat", pch = 15, cex = 0.6, add = TRUE)
plot(traps(ch[[2]]), add = TRUE, pch = 16)

# d to shore
plot(island_poly)
plot(masklist[[1]], covariate = "d.to.shore", pch = 15, cex = 0.6, add = TRUE)
plot(traps(ch[[1]]), add = TRUE, pch = 16)

plot(island_poly)
plot(masklist[[2]], covariate = "d.to.shore", pch = 15, cex = 0.6, add = TRUE)
plot(traps(ch[[2]]), add = TRUE, pch = 16)

# d to road
plot(island_poly)
plot(masklist[[1]], covariate = "d.to.road", pch = 15, cex = 0.6, add = TRUE)

azplot(traps(ch[[1]]), add = TRUE, pch = 16)

plot(island_poly)
plot(masklist[[2]], covariate = "d.to.road", pch = 15, cex = 0.6, add = TRUE)
plot(roads, add = TRUE)
plot(traps(ch[[2]]), add = TRUE, pch = 16)

# high MLA activity areas
plot(island_poly)
plot(masklist[[1]], covariate = "MLA", pch = 15, cex = 0.6, add = TRUE)
plot(traps(ch[[1]]), add = TRUE, pch = 16)

plot(island_poly)
plot(masklist[[2]], covariate = "MLA", pch = 15, cex = 0.6, add = TRUE)
plot(traps(ch[[2]]), add = TRUE, pch = 16)

# distance to high MLA activity areas
plot(island_poly)
plot(masklist[[1]], covariate = "d.to.MLA", pch = 15, cex = 0.6, add = TRUE)
plot(traps(ch[[1]]), add = TRUE, pch = 16)

plot(island_poly)
plot(masklist[[2]], covariate = "d.to.MLA", pch = 15, cex = 0.6, add = TRUE)
plot(traps(ch[[2]]), add = TRUE, pch = 16)

# elevation
plot(island_poly)
plot(masklist[[1]], covariate = "elev", pch = 15, cex = 0.6, add = TRUE)
plot(traps(ch[[1]]), add = TRUE, pch = 16)

plot(island_poly)
plot(masklist[[2]], covariate = "elev", pch = 15, cex = 0.6, add = TRUE)
plot(traps(ch[[2]]), add = TRUE, pch = 16)

# 8. save mask..............
saveRDS(masklist, file = "masklist.rds")
masklist <- readRDS("masklist.rds")

# 8a. checking mismatch of traps and mask.........
plot(masklist) #looks like session 2
plot(traps(ch[[1]]), add = TRUE, col = "red", pch = 16)
plot(traps(ch[[2]]), add = TRUE, col = "red", pch = 16)

st_crs(tinian) 
st_crs(traps.sf) #good both are the same projection

#actual plots
#session 1
plot(island_sp)
plot(masklist[[1]], add = TRUE) 
plot(traps(ch[[1]]), add = TRUE, col = "red", pch = 16)

#session 2
plot(island_sp)
plot(masklist[[2]], add = TRUE) 
plot(traps(ch[[2]]), add = TRUE, col = "red", pch = 16)

#check that masks cover the camera traps.......
# session 1
tr1 <- as.data.frame(traps(ch[[1]]))

xr <- range(masklist[[1]]$x)
yr <- range(masklist[[1]]$y)

tr1$inside_extent <-
  tr1$x >= xr[1] &
  tr1$x <= xr[2] &
  tr1$y >= yr[1] &
  tr1$y <= yr[2]

table(tr1$inside_extent) # good

# session 2
tr2 <- as.data.frame(traps(ch[[2]]))

xr <- range(masklist[[2]]$x)
yr <- range(masklist[[2]]$y)

tr2$inside_extent <-
  tr2$x >= xr[1] &
  tr2$x <= xr[2] &
  tr2$y >= yr[1] &
  tr2$y <= yr[2]

table(tr2$inside_extent) # good


# 9. try modelling..............
# Trying null models............................................................
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

# Trying D covariates...........................................................
# habitat model...................
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

summary(mDhabitat) #SE for some still pretty large....
AIC(m0, mDhabitat) #habitat is better > 10 AIC

saveRDS(mDhabitat, file = "mDhabitat.rds")
mDhabitat <- readRDS("mDhabitat.rds")

#try graphing
#session 1
hold=predictDsurface(mDhabitat, mask = masklist[[1]], se.D = FALSE, cl.D = FALSE, alpha =0.05)
plot(hold)   #this one is boring cause we have no variation on D
plot(traps(ch[[1]]),  add = TRUE, col = "red", pch = 16)

#session 2
hold=predictDsurface(mDhabitat, mask = masklist[[2]], se.D = FALSE, cl.D = FALSE, alpha =0.05)
plot(hold)   #this one is boring cause we have no variation on D
plot(traps(ch[[2]]),  add = TRUE, col = "red", pch = 16)

esaPlot(mDhabitat) #buffer size still ok 

# session model .......................
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

# distance to shore model ......................................................
#first scale the covariate
for(i in 1:2) {
  
  cv <- covariates(masklist[[i]])
  
  cv$d.to.shore <- as.numeric(
    scale(cv$d.to.shore)
  )
  
  covariates(masklist[[i]]) <- cv
}

#check that they are scaled
summary(covariates(masklist[[1]])$d.to.shore)
summary(covariates(masklist[[2]])$d.to.shore)

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
hold2=predictDsurface(mDshore, mask = masklist, se.D = FALSE, cl.D = FALSE, alpha =0.05)
plot(hold2)  

#session 1
hold=predictDsurface(mDshore, mask = masklist[[1]], se.D = FALSE, cl.D = FALSE, alpha =0.05)
plot(island_poly)
plot(hold, add = TRUE)  
plot(traps(ch[[1]]),  add = TRUE, col = "red", pch = 16)

#session 2
hold=predictDsurface(mDshore, mask = masklist[[2]], se.D = FALSE, cl.D = FALSE, alpha =0.05)
plot(island_poly)
plot(hold, add = TRUE)  
plot(traps(ch[[2]]),  add = TRUE, col = "red", pch = 16)

esaPlot(mDshore) # good
plot(mDshore)

# estimating abundance & avg density in MLA ~~~~~~~~~~~~~~~~~~~~~~
MLA_boundary <- st_read("C:\\Users\\celin\\OneDrive\\Desktop\\Tinian_GIS_layers\\MLA_boundary\\MLA_Boundary_2025.shp")

crs(MLA_boundary)

#reproject to UTM 55N
MLA_boundary <- st_transform(MLA_boundary, 32655)
#MLA_boundary <- as(MLA_boundary, "Spatial")

plot(island_poly)
plot(MLA_boundary, add = TRUE, border = "red", lwd = 2)

#extract mask coordinates.....
mask_xy <- st_as_sf(
  as.data.frame(masklist[[1]]),
  coords = c("x", "y"),
  crs = st_crs(MLA_boundary)
)

inside <- lengths(st_within(mask_xy, MLA_boundary)) > 0

mask_MLA <- masklist[[1]][inside, ]

covariates(mask_MLA) <- covariates(masklist[[1]])[inside, ]

summary(mask_MLA)
head(covariates(mask_MLA))

region.N(
  mDshore,
  region = mask_MLA,
  spacing = 100)

#calculate abundance in MLA
N_MLA <- region.N(
  mDshore,
  region = MLA_boundary,
  spacing = 250)

#trial of activity centers: session 1.... not session specific -> will need to improve
xy <- as.data.frame(masklist[[1]])
get_ac <- function(p, xy) {
  w <- p / sum(p, na.rm = TRUE)
  
  xhat <- sum(xy$x * w)
  yhat <- sum(xy$y * w)
  
  c(x = xhat, y = yhat)
}
ac <- t(sapply(fx, get_ac, xy = xy))
head(ac)

plot(masklist[[1]])
points(ac[,1], ac[,2], pch = 16, col = "blue")
plot(traps(ch[[1]]),  add = TRUE, col = "red", pch = 16)


# distance to nearest road model ..............................................
#first scale the covariate
for(i in 1:2) {
  
  cv <- covariates(masklist[[i]])
  
  cv$d.to.road <- as.numeric(
    scale(cv$d.to.road)
  )
  
  covariates(masklist[[i]]) <- cv
}

#check that they are scaled
summary(covariates(masklist[[1]])$d.to.road)
summary(covariates(masklist[[2]])$d.to.road)

mDroad <- secr.fit(
  ch,
  mask = masklist,
  model = list(
    D ~ d.to.road,
    g0 ~ 1,
    sigma ~ 1
  ),
  detectfn = "halfnormal"
)

summary(mDroad)
AIC(m0, mDhabitat, mDsession, mDshore, mDroad)

saveRDS(mDroad, file = "mDroad.rds")
mDroad <- readRDS("mDroad.rds")

# Inside/outside MLA activity areas model ...............
mDMLA <- secr.fit(
  ch,
  mask = masklist,
  model = list(
    D ~ MLA,
    g0 ~ 1,
    sigma ~ 1
  ),
  detectfn = "halfnormal"
)

summary(mDMLA)
AIC(m0, mDhabitat, mDsession, mDshore, mDroad, mDMLA)

saveRDS(mDMLA, file = "mDMLA.rds")
mDMLA <- readRDS("mDMLA.rds")

# Distance to MLA activity areas model ...............
#first scale the covariate
for(i in 1:2) {
  
  cv <- covariates(masklist[[i]])
  
  cv$d.to.MLA <- as.numeric(
    scale(cv$d.to.MLA)
  )
  
  covariates(masklist[[i]]) <- cv
}

#check that they are scaled
summary(covariates(masklist[[1]])$d.to.MLA)
summary(covariates(masklist[[2]])$d.to.MLA)

mDd.to.MLA <- secr.fit(
  ch,
  mask = masklist,
  model = list(
    D ~ d.to.MLA,
    g0 ~ 1,
    sigma ~ 1
  ),
  detectfn = "halfnormal"
)

summary(mDd.to.MLA)
AIC(m0, mDhabitat, mDsession, mDshore, mDroad, mDMLA, mDd.to.MLA)

saveRDS(mDd.to.MLA, file = "mDd.to.MLA.rds")
mDd.to.MLA <- readRDS("mDd.to.MLA.rds")


# distance to shore ^2.............................

#check that they are scaled
summary(covariates(masklist[[1]])$d.to.shore)
summary(covariates(masklist[[2]])$d.to.shore)

mDshoresq <- secr.fit(
  ch,
  mask = masklist,
  model = list(
    D ~ d.to.shore + I(d.to.shore^2),
    g0 ~ 1,
    sigma ~ 1
  ),
  detectfn = "halfnormal"
)

summary(mDshoresq)
AIC(m0, mDhabitat, mDsession, mDshore, mDroad, mDMLA, mDd.to.MLA, mDshoresq)

saveRDS(mDshoresq, file = "mDshoresq.rds")
mDshoresq <- readRDS("mDshoresq.rds")

# distance to shore ^3...................
summary(covariates(masklist[[1]])$d.to.shore)
summary(covariates(masklist[[2]])$d.to.shore)

mDshorecubed <- secr.fit(
  ch,
  mask = masklist,
  model = list(
    D ~ d.to.shore + I(d.to.shore^2) + I(d.to.shore^3),
    g0 ~ 1,
    sigma ~ 1
  ),
  detectfn = "halfnormal"
)

summary(mDshorecubed)
AIC(m0, mDhabitat, mDsession, mDshore, mDroad, mDMLA, mDd.to.MLA, mDshoresq, mDshorecubed)

saveRDS(mDshorecubed, file = "mDshorecubed.rds")
mDshorecubed <- readRDS("mDshorecubed.rds")

# distance to road ^2.............................
summary(covariates(masklist[[1]])$d.to.road)
summary(covariates(masklist[[2]])$d.to.road)

#mDroadsq <- secr.fit(
 # ch,
#  mask = masklist,
 # model = list(
  #  D ~ d.to.road + I(d.to.road^2),
  #  g0 ~ 1,
#    sigma ~ 1
 # ),
 # detectfn = "halfnormal"
#) #failed.....

# distance to shore + distance to road.................................
#check that they are scaled
summary(covariates(masklist[[1]])$d.to.shore)
summary(covariates(masklist[[2]])$d.to.shore)

summary(covariates(masklist[[1]])$d.to.road)
summary(covariates(masklist[[2]])$d.to.road)

mDshore.road <- secr.fit(
  ch,
  mask = masklist,
  model = list(
    D ~ d.to.shore + d.to.road,
    g0 ~ 1,
    sigma ~ 1
  ),
  detectfn = "halfnormal"
)

summary(mDshore.road)
AIC(m0, mDhabitat, mDsession, mDshore, mDroad, mDMLA, mDd.to.MLA, mDshoresq, mDshore.road, mDshorecubed)

saveRDS(mDshore.road, file = "mDshore.road.rds")
mDshore.road <- readRDS("mDshore.road.rds")

# elevation ............................................
summary(covariates(masklist[[1]])$elev)
summary(covariates(masklist[[2]])$elev)

# scale elevation per session
for(i in 1:2) {
  
  cv <- covariates(masklist[[i]])
  
  cv$elev <- as.numeric(
    scale(cv$elev)
  )
  
  covariates(masklist[[i]]) <- cv
}

summary(covariates(masklist[[1]])$elev) 
summary(covariates(masklist[[2]])$elev)

mDelev <- secr.fit(
  ch,
  mask = masklist,
  model = list(
    D ~ elev,
    g0 ~ 1,
    sigma ~ 1
  ),
  detectfn = "halfnormal"
)

summary(mDelev)
AIC(m0, mDhabitat, mDsession, mDshore, mDroad, mDMLA, mDd.to.MLA, 
    mDshoresq, mDshorecubed, mDelev)

saveRDS(mDelev, file = "mDelev.rds")
mDelev <- readRDS("mDelev.rds")

# slope ............................................
summary(covariates(masklist[[1]])$slope)
summary(covariates(masklist[[2]])$slope)

# scale slope per session
for(i in 1:2) {
  
  cv <- covariates(masklist[[i]])
  
  cv$slope <- as.numeric(
    scale(cv$slope)
  )
  
  covariates(masklist[[i]]) <- cv
}

summary(covariates(masklist[[1]])$slope) 
summary(covariates(masklist[[2]])$slope)


mDslope <- secr.fit(
  ch,
  mask = masklist,
  model = list(
    D ~ slope,
    g0 ~ 1,
    sigma ~ 1
  ),
  detectfn = "halfnormal"
)

summary(mDslope)
AIC(m0, mDhabitat, mDsession, mDshore, mDroad, mDMLA, mDd.to.MLA, 
    mDshoresq, mDshorecubed, mDelev, mDslope)

saveRDS(mDslope, file = "mDslope.rds")
mDslope <- readRDS("mDslope.rds") 

# elevation squared...............
summary(covariates(masklist[[1]])$elev) 
summary(covariates(masklist[[2]])$elev)

mDelevsq <- secr.fit(
  ch,
  mask = masklist,
  model = list(
    D ~ elev + I(elev^2),
    g0 ~ 1,
    sigma ~ 1
  ),
  detectfn = "halfnormal"
)

summary(mDelevsq)
AIC(m0, mDhabitat, mDsession, mDshore, mDroad, mDMLA, mDd.to.MLA, 
    mDshoresq, mDshorecubed, mDelev, mDelevsq)

saveRDS(mDelevsq, file = "mDelevsq.rds")
mDelevsq <- readRDS("mDelevsq.rds")

# distance from human areas ....................................................
# human activity areas
summary(covariates(masklist[[1]])$d.to.humans)
summary(covariates(masklist[[2]])$d.to.humans)

# scale distance to human activity areas
for(i in 1:2) {
  
  cv <- covariates(masklist[[i]])
  
  cv$d.to.humans <- as.numeric(
    scale(cv$d.to.humans)
  )
  
  covariates(masklist[[i]]) <- cv
}

summary(covariates(masklist[[1]])$d.to.humans) 
summary(covariates(masklist[[2]])$d.to.humans)

mDhumans <- secr.fit(
  ch,
  mask = masklist,
  model = list(
    D ~ d.to.humans,
    g0 ~ 1,
    sigma ~ 1
  ),
  detectfn = "halfnormal"
)

summary(mDhumans)
AIC(m0, mDhabitat, mDsession, mDshore, mDroad, mDMLA, mDd.to.MLA, 
    mDshoresq, mDshorecubed, mDelev, mDhumans)

saveRDS(mDhumans, file = "mDhumans.rds")
mDhumans <- readRDS("mDhumans.rds")

# check what's available 
summary(covariates(masklist[[1]]))

# Airport
summary(covariates(masklist[[1]])$d.to.Airport)
summary(covariates(masklist[[2]])$d.to.Airport)

for(i in 1:2) {
  
  cv <- covariates(masklist[[i]])
  
  cv$d.to.Airport <- as.numeric(
    scale(cv$d.to.Airport)
  )
  
  covariates(masklist[[i]]) <- cv
}

summary(covariates(masklist[[1]])$d.to.Airport) 
summary(covariates(masklist[[2]])$d.to.Airport)

mDAirport <- secr.fit(
  ch,
  mask = masklist,
  model = list(
    D ~ d.to.Airport,
    g0 ~ 1,
    sigma ~ 1
  ),
  detectfn = "halfnormal"
)

summary(mDAirport)
AIC(m0, mDhabitat, mDsession, mDshore, mDroad, mDMLA, mDd.to.MLA, 
    mDshoresq, mDshorecubed, mDelev, mDhumans, mDAirport)

saveRDS(mDAirport, file = "mDAirport.rds")
mDAirport <- readRDS("mDAirport.rds")

# Camp Tinian
summary(covariates(masklist[[1]])$d.to.CampTinian)
summary(covariates(masklist[[2]])$d.to.CampTinian)

for(i in 1:2) {
  
  cv <- covariates(masklist[[i]])
  
  cv$d.to.CampTinian <- as.numeric(
    scale(cv$d.to.CampTinian)
  )
  
  covariates(masklist[[i]]) <- cv
}

summary(covariates(masklist[[1]])$d.to.CampTinian) 
summary(covariates(masklist[[2]])$d.to.CampTinian)

mDCampTinian <- secr.fit(
  ch,
  mask = masklist,
  model = list(
    D ~ d.to.CampTinian,
    g0 ~ 1,
    sigma ~ 1
  ),
  detectfn = "halfnormal"
)

summary(mDCampTinian)
AIC(m0, mDhabitat, mDsession, mDshore, mDroad, mDMLA, mDd.to.MLA, 
    mDshoresq, mDshorecubed, mDelev, mDhumans, mDAirport,
    mDCampTinian)

saveRDS(mDCampTinian, file = "mDCampTinian.rds")
mDCampTinian <- readRDS("mDCampTinian.rds")

# Dump
summary(covariates(masklist[[1]])$d.to.Dump)
summary(covariates(masklist[[2]])$d.to.Dump)

for(i in 1:2) {
  
  cv <- covariates(masklist[[i]])
  
  cv$d.to.Dump <- as.numeric(
    scale(cv$d.to.Dump)
  )
  
  covariates(masklist[[i]]) <- cv
}

summary(covariates(masklist[[1]])$d.to.Dump) 
summary(covariates(masklist[[2]])$d.to.Dump)

mDDump <- secr.fit(
  ch,
  mask = masklist,
  model = list(
    D ~ d.to.Dump,
    g0 ~ 1,
    sigma ~ 1
  ),
  detectfn = "halfnormal"
)

summary(mDDump)
AIC(m0, mDhabitat, mDsession, mDshore, mDroad, mDMLA, mDd.to.MLA, 
    mDshoresq, mDshorecubed, mDelev, mDhumans, mDAirport,
    mDCampTinian, mDDump)

saveRDS(mDDump, file = "mDDump.rds")
mDDump <- readRDS("mDDump.rds")

# North Field
summary(covariates(masklist[[1]])$d.to.NorthField)
summary(covariates(masklist[[2]])$d.to.NorthField)

for(i in 1:2) {
  
  cv <- covariates(masklist[[i]])
  
  cv$d.to.NorthField <- as.numeric(
    scale(cv$d.to.NorthField)
  )
  
  covariates(masklist[[i]]) <- cv
}

summary(covariates(masklist[[1]])$d.to.NorthField) 
summary(covariates(masklist[[2]])$d.to.NorthField)

mDNF <- secr.fit(
  ch,
  mask = masklist,
  model = list(
    D ~ d.to.NorthField,
    g0 ~ 1,
    sigma ~ 1
  ),
  detectfn = "halfnormal"
)

summary(mDNF)
AIC(m0, mDhabitat, mDsession, mDshore, mDroad, mDMLA, mDd.to.MLA, 
    mDshoresq, mDshorecubed, mDelev, mDhumans, mDAirport,
    mDCampTinian, mDDump, mDNF)

saveRDS(mDNF, file = "mDNF.rds")
mDNF <- readRDS("mDNF.rds")

# Quarry
summary(covariates(masklist[[1]])$d.to.Quarry)
summary(covariates(masklist[[2]])$d.to.Quarry)

for(i in 1:2) {
  
  cv <- covariates(masklist[[i]])
  
  cv$d.to.Quarry <- as.numeric(
    scale(cv$d.to.Quarry)
  )
  
  covariates(masklist[[i]]) <- cv
}

summary(covariates(masklist[[1]])$d.to.Quarry) 
summary(covariates(masklist[[2]])$d.to.Quarry)

mDQuarry <- secr.fit(
  ch,
  mask = masklist,
  model = list(
    D ~ d.to.Quarry,
    g0 ~ 1,
    sigma ~ 1
  ),
  detectfn = "halfnormal"
)

summary(mDQuarry)
AIC(m0, mDhabitat, mDsession, mDshore, mDroad, mDMLA, mDd.to.MLA, 
    mDshoresq, mDshorecubed, mDelev, mDhumans, mDAirport,
    mDCampTinian, mDDump, mDNF, mDQuarry)

saveRDS(mDQuarry, file = "mDQuarry.rds")
mDQuarry <- readRDS("mDQuarry.rds")

# Town 
summary(covariates(masklist[[1]])$d.to.Town)
summary(covariates(masklist[[2]])$d.to.Town)

for(i in 1:2) {
  
  cv <- covariates(masklist[[i]])
  
  cv$d.to.Town <- as.numeric(
    scale(cv$d.to.Town)
  )
  
  covariates(masklist[[i]]) <- cv
}

summary(covariates(masklist[[1]])$d.to.Town) 
summary(covariates(masklist[[2]])$d.to.Town)

mDTown <- secr.fit(
  ch,
  mask = masklist,
  model = list(
    D ~ d.to.Town,
    g0 ~ 1,
    sigma ~ 1
  ),
  detectfn = "halfnormal"
)

summary(mDTown)
AIC(m0, mDhabitat, mDsession, mDshore, mDroad, mDMLA, mDd.to.MLA, 
    mDshoresq, mDshorecubed, mDelev, mDhumans, mDAirport,
    mDCampTinian, mDDump, mDNF, mDQuarry, mDTown)

saveRDS(mDTown, file = "mDTown.rds")
mDTown <- readRDS("mDTown.rds")

# VOA
summary(covariates(masklist[[1]])$d.to.VOA)
summary(covariates(masklist[[2]])$d.to.VOA)

for(i in 1:2) {
  
  cv <- covariates(masklist[[i]])
  
  cv$d.to.VOA <- as.numeric(
    scale(cv$d.to.VOA)
  )
  
  covariates(masklist[[i]]) <- cv
}

summary(covariates(masklist[[1]])$d.to.VOA) 
summary(covariates(masklist[[2]])$d.to.VOA)

mDVOA <- secr.fit(
  ch,
  mask = masklist,
  model = list(
    D ~ d.to.VOA,
    g0 ~ 1,
    sigma ~ 1
  ),
  detectfn = "halfnormal"
)

summary(mDVOA)
AIC(m0, mDhabitat, mDsession, mDshore, mDroad, mDMLA, mDd.to.MLA, 
    mDshoresq, mDshorecubed, mDelev, mDhumans, mDAirport,
    mDCampTinian, mDDump, mDNF, mDQuarry, mDTown, mDVOA,
    mDslope, mDelevsq)

saveRDS(mDVOA, file = "mDVOA.rds")
mDVOA <- readRDS("mDVOA.rds")


# trying shore + elevation model......................
mDshore.elev <- secr.fit(
  ch,
  mask = masklist,
  model = list(
    D ~ d.to.shore + elev,
    g0 ~ 1,
    sigma ~ 1
  ),
  detectfn = "halfnormal"
)

summary(mDshore.elev)

saveRDS(mDshore.elev, file = "mDshore.elev.rds")
mDshore.elev <- readRDS("mDshore.elev.rds")


# final comparison of desired models
AIC(m0, mDhabitat, mDsession, mDshore, mDroad, mDshore.road, mDd.to.MLA, 
    mDshoresq, mDshorecubed, mDelev, mDelevsq, mDhumans, mDAirport,
    mDCampTinian, mDDump, mDNF, mDQuarry, mDTown, mDVOA,
    mDslope, mDshore.elev)


# Trying g0 covariates..........................................................
# g0 ~ site_habitat
#mg0habitat <- secr.fit(
 # ch,
  #mask = masklist,
  #model = list(
   # D ~ 1,
    #g0 ~ site_habitat,
    #sigma ~ 1
  #),
  #detectfn = "halfnormal"
#) #fails b/c need same covariate levels in each session (do not match rn)

# remove ironwood
# recode Casuarina -> Mixed Introduced Forest
for (s in seq_along(ch)) {
  
  tr <- traps(ch[[s]])
  covs <- covariates(tr)
  
  covs$site_habitat[
    covs$site_habitat == "Casuarina Thicket"
  ] <- "Mixed Introduced Forest"
  
  covariates(tr) <- covs
  traps(ch[[s]]) <- tr
}

# get all site_habitat levels after recoding
all_levels <- sort(unique(unlist(lapply(ch, function(x) {
  covariates(traps(x))$site_habitat
}))))

# enforce same levels in each session
for (s in seq_along(ch)) {
  
  tr <- traps(ch[[s]])
  covs <- covariates(tr)
  
  covs$site_habitat <- factor(
    covs$site_habitat,
    levels = all_levels
  )
  
  covariates(tr) <- covs
  traps(ch[[s]]) <- tr
}

#check it worked 
lapply(ch, function(x)
  levels(covariates(traps(x))$site_habitat)
)

# try refitting the model
mg0habitat <- secr.fit(
  ch,
  mask = masklist,
  model = list(
    D ~ 1,
    g0 ~ site_habitat,
    sigma ~ 1
  ),
  detectfn = "halfnormal"
)

summary(mg0habitat)
AIC(m0, mDhabitat, mDsession, mDshore) #not comparable 
AIC(m0, mg0habitat) #lower AIC but somehow still not comparable


saveRDS(mg0habitat, file = "mg0habitat.rds")
mg0habitat <- readRDS("mg0habitat.rds")

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

# Inspect ch ###################################################################
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
