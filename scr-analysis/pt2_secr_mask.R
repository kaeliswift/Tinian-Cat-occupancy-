# Formatting of mask data for secr analysis....................................

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

# 1. Read in ch #################################################################
ch <- readRDS("m_ch.RDS")
summary(ch) #good


# 2. Read habitat shapefile  #####################################
# you will have to download and direct R to your GIS layers (they are too big to store in the repo)
tinian <- st_read(
  "C:/Users/celin/OneDrive/Desktop/Tinian_GIS_layers/CNMI Hi-Res veg data/tinian_release.shp")

# make sure CRS matches traps
st_crs(tinian)

# dissolve polygons into single island boundary
island_boundary <- st_union(tinian)

plot((island_boundary)) #shows island boundary

# create spatial polygon.... more clear for secr
# convert island_boundary to SpatVect
island_poly <- vect(island_boundary)


# 3. Reclassify habitat ##########################################
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
# will have to fill the NAs 


# 4. Rasterize habitat ###########################################
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

# 5. Read in more covariates #################################

# define shoreline...............
shoreline <- st_boundary(island_boundary)

st_crs(shoreline) == st_crs(tinian) #good

plot(shoreline)

# define roads................... 
roads <- st_read(
  "C:/Users/celin/OneDrive/Desktop/Tinian_GIS_layers/Tinian_roads/Roads Tinian.shp")

roads <- st_geometry(roads)
plot(roads)

st_crs(roads) == st_crs(tinian) #bad
roads <- st_transform(roads, st_crs(tinian))
st_crs(roads) == st_crs(tinian) #good

# define MLA activity areas....................
MLA_activity <- st_read(
  "C:/Users/celin/OneDrive/Desktop/Tinian_GIS_layers/MLA_activity_areas/MLA_activity areas.shp")

st_crs(MLA_activity) == st_crs(tinian) # looks ok

plot(island_poly)
plot(st_geometry(MLA_activity), add = TRUE, col = "red")

MLA_activity <- st_geometry(MLA_activity)


# extract elevation............................
elev <- rast("C:/Users/celin/OneDrive/Desktop/Tinian_GIS_layers/tinian_dem")

st_crs(elev) == st_crs(tinian) # bad

st_crs(elev) #makes sure to add EPSG
crs(elev) <- "EPSG:32655"

st_crs(elev) == st_crs(tinian) # looks ok

plot(elev)

# extract slope from elevation...............
slope <- terrain(
  elev,
  v = "slope",
  unit = "degrees",
  neighbors = 8
) #THIS WILL BE SLOW


# extract human areas separately & together..................
humans <- st_read(
  "C:/Users/celin/OneDrive/Desktop/Tinian_GIS_layers/Human_activity/humans.shp")

# plot humans covariate
plot(st_geometry(island_boundary))
plot(st_geometry(humans), add = TRUE, col = "pink")

table(humans$Name) #available human locations

# plot human areas separately
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

# 6. Create masks per session w/ covariates  ##################################

# create session masks..................................................................
masklist <- lapply(
  1:2,
  function(i){
    
    # BUILD TRAPBUFFER MASK..........
    
    m <- make.mask(
      traps(ch[[i]]),
      type = "trapbuffer",
      buffer = 7000,
      spacing = 250, 
      poly = island_poly
    )
    
    # define coordinates
    xy <- cbind(m$x, m$y)
    
    # HABITAT.............
    
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
    
    # DISTANCE TO SHORE & ROADS.........
    
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
    
    
    # MLA ACTIVITY ZONES -- no buffer around area rn.... may need to change
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
    
    # ELEVATION.......
    
    elev_vals <- terra::extract(
      elev,
      xy
    )
    
    covariates(m)$elev <- elev_vals$tinian_dem
    
    # SLOPE..............
    slope_vals <- terra::extract(slope, xy)
    
    covariates(m)$slope <- slope_vals[,1]
    
    
    # DISTANCE TO HUMAN ACTIVITY AREAS.........
    
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

# 7. Inspect mask points ######################################################
verify(masklist) #fails b/c it is a list of 2 masks

verify(masklist[[1]]) #good
verify(masklist[[2]]) #good

# force secr to recognize multi-session mask
class(masklist) <- c("mask", "list")
verify(masklist)

summary(masklist) #NAs in habitat & elev & slope ---- will need to fix

nrow(masklist[[1]]) #1580 pts in session 1
nrow(masklist[[2]]) #945 pts in session 1

summary(covariates(masklist[[1]]))
summary(covariates(masklist[[2]]))

# check habitat 
summary(covariates(masklist[[1]])$habitat)
summary(covariates(masklist[[2]])$habitat) #will need to reassign NAs

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
verify(masklist) 

# check all covariates there
names(covariates(masklist[[1]]))
names(covariates(masklist[[2]]))

# 8. Replace and NAs with nearest neighbor #############################################for (i in seq_along(masklist)) {
for (i in seq_along(masklist)) {
  
  covs <- covariates(masklist[[i]])
  
  h <- covs$habitat   #habitat
  mode <- names(which.max(table(h)))
  h[is.na(h)] <- mode
  covs$habitat <- droplevels(h)
  
  covs$elev[is.na(covs$elev)] <- median(covs$elev, na.rm = TRUE) #elevation
  covs$slope[is.na(covs$slope)] <- median(covs$slope, na.rm = TRUE) #slope
  
  covariates(masklist[[i]]) <- covs
}

# check the NAs are gone
for (i in seq_along(masklist)) {
  cat("\nSession", i, "\n")
  
  print(colSums(is.na(covariates(masklist[[i]]))))
}


# 8. Check covariate correlations  ############################################
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
) #not good r = 0.67 ---> exclude d.to.shore + elev model from analysis


# numeric covariate correlation matrix
num_cov <- covariates(masklist[[i]])[sapply(covariates(masklist[[i]]), is.numeric)]

cor(num_cov, use="complete.obs")

# 9. Scale covariates across sessions #########################################
# use an average scale for both sessions so 1 SD means the same for both sessions

scale_covariates <- c(
  "d.to.shore",
  "d.to.road",
  "elev",
  "slope",
  "d.to.MLA",
  "d.to.humans",
  "d.to.Airport",
  "d.to.CampTinian",
  "d.to.Dump",
  "d.to.NorthField",
  "d.to.Quarry",
  "d.to.Town",
  "d.to.VOA"
)

for (v in scale_covariates) {
  
  # Combine values from all sessions
  all_vals <- unlist(lapply(masklist, function(m) covariates(m)[[v]]))
  
  # Global mean and SD
  mu <- mean(all_vals, na.rm = TRUE)
  sigma <- sd(all_vals, na.rm = TRUE)
  
  # Apply the same scaling to every session
  for (i in seq_along(masklist)) {
    
    covs <- covariates(masklist[[i]])
    covs[[paste0(v, "_z")]] <- (covs[[v]] - mu) / sigma
    covariates(masklist[[i]]) <- covs
  }
}

# check that it worked: 
summary(covariates(masklist[[1]])$d.to.shore_z)
summary(covariates(masklist[[2]])$d.to.shore_z)

for (v in scale_covariates) {
  all_z <- unlist(lapply(masklist, function(m) covariates(m)[[paste0(v, "_z")]]))
  cat(v,
      "mean =", round(mean(all_z), 6),
      "sd =", round(sd(all_z), 6), "\n")
} # you want mean ~ 0 and sd ~ 1

# 10. Plot each session mask  ################################################
plot(island_poly)
plot(masklist[[1]], pch = 15, cex = 0.3, add = TRUE)
plot(traps(ch[[1]]), add = TRUE)

plot(island_poly)
plot(masklist[[2]], pch = 15, cex = 0.3, add = TRUE)
plot(traps(ch[[2]]), add = TRUE)


# double check mask structure
class(masklist[[1]])
class(masklist[[2]])

attributes(masklist[[1]])
attributes(masklist[[2]])


# 11. Graph mask covariates  ##################################################

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

# 12. Save mask as rds object  ###################################################
saveRDS(masklist, file = "masklist.rds")
masklist <- readRDS("masklist.rds")

# 13. Check for mismatch of traps and mask  #####################################
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