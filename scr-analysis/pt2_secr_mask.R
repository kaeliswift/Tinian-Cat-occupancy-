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
ch <- readRDS("ch.RDS")
summary(ch) #good

# 2. Read habitat shapefile  #####################################
# you will have to download and direct R to your GIS layers (they are too big to store in the repo)
tinian <- st_read(
  "C:/Users/celin/OneDrive/Desktop/Tinian_GIS_layers/CNMI Hi-Res veg data/amidon_2016_tinian.shp")

# make sure CRS matches traps
st_crs(tinian)

# dissolve polygons into single island boundary
island_boundary <- st_union(tinian)

# create spatial polygon.... more clear for secr
# convert island_boundary to SpatVect
island_poly <- vect(island_boundary)

plot(island_poly)  #shows island boundary

# 3. Reclassify habitat ##########################################
unique(tinian$VegClass)

tinian <- tinian %>%
  mutate(
    habitat = case_when(
      grepl("Leucaena", VegClass) ~ "tangantangan",
      grepl("Mixed Introduced", VegClass) ~ "mixed_introduced",
      grepl("Casuarina", VegClass) ~ "mixed_introduced", #ironwood is included in mixed_introduced
      grepl("Mixed Grass/Herbaceous", VegClass) ~ "shrub_grass",
      grepl("Native", VegClass) ~ "native_limestone",
      TRUE ~ NA_character_ #leave everything else as NAs to be filled with nearest neighbor
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

# 5. Read in more covariates ###################################################

# define shoreline..........................
shoreline <- st_boundary(island_boundary)
plot(shoreline)

# define roads............................. 
roads <- st_read(
  "C:/Users/celin/OneDrive/Desktop/Tinian_GIS_layers/Tinian_roads/Roads Tinian.shp")

roads <- st_geometry(roads)
plot(roads)

st_crs(roads) == st_crs(tinian) #bad
roads <- st_transform(roads, st_crs(tinian))
st_crs(roads) == st_crs(tinian) #good

# define MLA activity areas.................
MLA_activity <- st_read(
  "C:/Users/celin/OneDrive/Desktop/Tinian_GIS_layers/MLA_activity_areas/MLA_activity areas.shp")

st_crs(MLA_activity) == st_crs(tinian) # looks ok

plot(island_poly)
plot(st_geometry(MLA_activity), add = TRUE, col = "red")

MLA_activity <- st_geometry(MLA_activity)

# extract elevation.........................
elev <- rast("C:/Users/celin/OneDrive/Desktop/Tinian_GIS_layers/tinian_dem")

st_crs(elev) == st_crs(tinian) # bad

st_crs(elev) #makes sure to add EPSG
crs(elev) <- "EPSG:32655"

st_crs(elev) == st_crs(tinian) # looks ok

plot(elev)

# extract slope from elevation.....................
slope <- terrain(
  elev,
  v = "slope",
  unit = "degrees",
  neighbors = 8
) #THIS WILL BE SLOW


# extract human areas separately & together..............................
humans <- st_read(
  "C:/Users/celin/OneDrive/Desktop/Tinian_GIS_layers/Human_activity/humans.shp")

st_crs(humans) == st_crs(tinian) # bad

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

# create humans covariate
humans <- st_geometry(humans)
# plot humans covariate
plot(island_poly)
plot(st_geometry(humans), add = TRUE, col = "pink")


# 6. Create mask w/ covariates  ################################################

mask <- make.mask(
  traps(ch),
  type = "trapbuffer",
  buffer = 8000,   # choose your final buffer
  spacing = 250,
  poly = island_poly
)


# define coordinates
xy <- cbind(mask$x, mask$y)


# HABITAT 

ex <- terra::extract(
  habitat_raster,
  xy
)

covariates(mask)$habitat <- factor(ex$habitat)

covariates(mask)$habitat <-
  relevel(
    covariates(mask)$habitat,
    ref = "tangantangan"
  )


# DISTANCE TO SHORE 

pts <- st_as_sf(
  data.frame(x = mask$x, y = mask$y),
  coords = c("x", "y"),
  crs = st_crs(tinian)
)


dshore <- st_distance(
  pts,
  shoreline
)

covariates(mask)$d.to.shore <- as.numeric(dshore)


# DISTANCE TO ROAD 

droads <- st_distance(
  pts,
  roads
)

covariates(mask)$d.to.road <-
  as.numeric(apply(droads, 1, min))


# MLA 

inside_MLA <- st_intersects(
  pts,
  MLA_activity,
  sparse = FALSE
)

covariates(mask)$MLA <- factor(
  ifelse(
    rowSums(inside_MLA) > 0,
    "inside",
    "outside"
  )
)


# distance to MLA

dMLA <- st_distance(
  pts,
  MLA_activity
)

covariates(mask)$d.to.MLA <-
  as.numeric(apply(dMLA, 1, min))


# ELEVATION 

elev_vals <- terra::extract(
  elev,
  xy
)

covariates(mask)$elev <-
  elev_vals$tinian_dem



# SLOPE 

slope_vals <- terra::extract(
  slope,
  xy
)

covariates(mask)$slope <-
  slope_vals$slope



# DISTANCE TO HUMAN AREAS 

dhumans <- st_distance(
  pts,
  humans
)

covariates(mask)$d.to.humans <-
  as.numeric(apply(dhumans, 1, min))


# Individual human areas

covariates(mask)$d.to.Airport <-
  as.numeric(apply(st_distance(pts, Airport),1,min))

covariates(mask)$d.to.CampTinian <-
  as.numeric(apply(st_distance(pts, CampTinian),1,min))

covariates(mask)$d.to.Dump <-
  as.numeric(apply(st_distance(pts, Dump),1,min))

covariates(mask)$d.to.NorthField <-
  as.numeric(apply(st_distance(pts, NorthField),1,min))

covariates(mask)$d.to.Quarry <-
  as.numeric(apply(st_distance(pts, Quarry),1,min))

covariates(mask)$d.to.Town <-
  as.numeric(apply(st_distance(pts, Town),1,min))

covariates(mask)$d.to.VOA <-
  as.numeric(apply(st_distance(pts, VOA),1,min))


# 7. Inspect mask points ######################################################
verify(mask)
summary(mask) #NAs in habitat & elev & slope ---- will need to fix

nrow(mask) #1610 mask points

plot(mask)

summary(covariates(mask))

# 8. Replace any NAs with nearest neighbor #####################################
# habitat
h <- covariates(mask)$habitat

h[is.na(h)] <-
  names(sort(table(h), decreasing=TRUE))[1]

covariates(mask)$habitat <- droplevels(h)


# elevation
covariates(mask)$elev[
  is.na(covariates(mask)$elev)
] <- median(
  covariates(mask)$elev,
  na.rm=TRUE
)


# slope
covariates(mask)$slope[
  is.na(covariates(mask)$slope)
] <- median(
  covariates(mask)$slope,
  na.rm=TRUE
)

# 9. Check covariate correlations  ############################################
# check numeric covariate correlation (r > 0.5 not good)

#d.to.shore
cor(
  covariates(mask)$d.to.shore,
  covariates(mask)$d.to.road
) #good

cor(
  covariates(mask)$d.to.shore,
  covariates(mask)$d.to.MLA
) #good

cor(
  covariates(mask)$d.to.shore,
  covariates(mask)$elev
) #good

cor(
  covariates(mask)$d.to.shore,
  covariates(mask)$slope
) #good

cor(
  covariates(mask)$d.to.shore,
  covariates(mask)$d.to.humans
) #good

#d.to.road
cor(
  covariates(mask)$d.to.road,
  covariates(mask)$d.to.MLA
) #good

cor(
  covariates(mask)$d.to.road,
  covariates(mask)$elev
) #good

cor(
  covariates(mask)$d.to.road,
  covariates(mask)$slope
) #good

cor(
  covariates(mask)$d.to.road,
  covariates(mask)$d.to.humans
) #good

#d.to.MLA
cor(
  covariates(mask)$d.to.MLA,
  covariates(mask)$elev
) #good

cor(
  covariates(mask)$d.to.MLA,
  covariates(mask)$slope
) #good

#elev
cor(
  covariates(mask)$elev,
  covariates(mask)$slope
) #good

cor(
  covariates(mask)$elev,
  covariates(mask)$d.to.humans
) #good

#slope
cor(
  covariates(mask)$slope,
  covariates(mask)$d.to.humans
) #good


# 10. Standardized numeric covariates #########################################
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
  covariates(mask)[[paste0(v, "_z")]] <-
    as.numeric(scale(covariates(mask)[[v]]))
}

summary(covariates(mask))

# check SD to make sure that it worked (SD of 0 is bad) 
sapply(covariates(mask), function(x) {
  if(is.numeric(x)) sd(x, na.rm=TRUE)
})


# numeric covariate correlation matrix
num_cov <- covariates(mask)[sapply(covariates(mask), is.numeric)]

cor(num_cov, use="complete.obs")

# 11. Plot   mask  #####################################################
plot(island_poly)
plot(mask, pch = 15, cex = 0.3, add = TRUE)
plot(traps(ch), add = TRUE, col = "red", pch = 16)


# double check mask structure
class(mask)

verify(mask)


# 20. Graph mask covariates  ##################################################

# habitat data
plot(island_poly)
plot(mask, covariate = "habitat", pch = 15, cex = 0.6, add = TRUE)
plot(traps(ch), add = TRUE, pch = 16)


# d to shore
plot(island_poly)
plot(mask, covariate = "d.to.shore", pch = 15, cex = 0.6, add = TRUE)
plot(traps(ch), add = TRUE, pch = 16)


# d to road
plot(island_poly)
plot(mask, covariate = "d.to.road", pch = 15, cex = 0.6, add = TRUE)
plot(traps(ch), add = TRUE, pch = 16)

# elevation
plot(island_poly)
plot(mask, covariate = "elev", pch = 15, cex = 0.6, add = TRUE)
plot(traps(ch), add = TRUE, pch = 16)

#slope
plot(island_poly)
plot(mask, covariate = "slope", pch = 15, cex = 0.6, add = TRUE)
plot(traps(ch), add = TRUE, pch = 16)

# 12. Save mask as rds object  ###################################################
saveRDS(mask, file = "rds")
mask <- readRDS("mask.rds")

# 13. Check for mismatch of traps and mask #####################################

# plot mask and traps together
plot(island_poly)
plot(mask, add = TRUE, pch = 15, cex = 0.4)
plot(traps(ch), add = TRUE, col = "red", pch = 16)


# check that mask covers all camera traps

tr <- as.data.frame(traps(ch))

xr <- range(mask$x)
yr <- range(mask$y)

tr$inside_extent <-
  tr$x >= xr[1] &
  tr$x <= xr[2] &
  tr$y >= yr[1] &
  tr$y <= yr[2]

table(tr$inside_extent)
