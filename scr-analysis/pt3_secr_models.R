# secr model analysis....................................

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

# 1. Read in needed objects #####################################

# capture history (ch) (script to create this in pt1_secr_formatting.R)
ch <- readRDS("ch.RDS")
verify(ch)  # should say: no errors found :-)

# mask (script to create this in pt2_secr_mask.R)
mask <- readRDS("mask.rds")
verify(mask)

# 2. Detection function models #############################
nullhalf <- secr.fit(ch,mask = mask,
                     model = list(D ~ 1,g0 ~ 1,sigma ~ 1),
                     detectfn = 0)

nullhaz <- secr.fit(ch,mask = mask,
                    model = list(D ~ 1,g0 ~ 1,sigma ~ 1),
                    detectfn = 1)

nullex <- secr.fit(ch,mask = mask,
                   model = list(D ~ 1,g0 ~ 1,sigma ~ 1),
                   detectfn = 2)

AIC(nullhalf, nullhaz, nullex) #going forward with hazard rate

# 3. Run h2 models ################

#null models #############################
null <- secr.fit(ch,mask = mask,
                 model = list(D ~ 1,g0 ~ 1,sigma ~ 1),
                 detectfn = 1)

gh2 <-secr.fit(ch,mask = mask,
               model = list(D ~ 1,g0 ~ h2,sigma ~ 1),
               detectfn = 1)

sh2 <- secr.fit(ch,mask = mask,
                model = list(D ~ 1,g0 ~ 1,sigma ~ h2),
                detectfn = 1)

gsh2 <- secr.fit(ch,mask = mask,
                 model = list(D ~ 1,g0 ~ h2,sigma ~ h2),
                 detectfn = 1)


AIC(null, gh2, gsh2, sh2) #gsh2 is best, move forward with it

# habitat model ######################
### FAILS ######################
# usually fails due to too many parameters, if you run detectfn = "halfnormal" it will work
Dhabitat <- secr.fit(ch,mask = mask,
                     model = list(D ~ habitat,g0 ~ h2,sigma ~ h2),
                     detectfn = "hazard rate")


# shore model ##################
Dshore <- secr.fit(ch,mask = mask,
                   model = list(D ~ d.to.shore_z,g0 ~ h2,sigma ~ h2),
                   detectfn = "hazard rate")

Dshoresq <- secr.fit(ch,mask = mask,
                     model = list(D ~ d.to.shore_z+ I(d.to.shore_z^2),g0~ h2,sigma ~ h2),
                     detectfn = "hazard rate")


# nearest road model #################################
Droad <- secr.fit(ch,mask = mask,
                  model = list(D ~ d.to.road_z, g0 ~ h2, sigma ~ h2),
                  detectfn = "hazard rate")


# shore + road model ######################
Dshore.road <- secr.fit(ch,mask = mask,
                        model = list(D ~ d.to.shore_z + d.to.road_z, g0 ~ h2, sigma ~ h2),
                        detectfn = "hazard rate")

# elevation model ######################################
Delev <- secr.fit(ch,mask = mask,
                  model = list(D ~ elev_z, g0 ~ h2, sigma ~ h2),
                  detectfn = "hazard rate")

# elev^2 model...............
Delevsq <- secr.fit(ch,mask = mask,
                    model = list(D ~ elev_z + I(elev_z^2), g0 ~ h2, sigma ~ h2),
                    detectfn = "hazard rate")


# slope model ######################################
Dslope <- secr.fit(ch, mask = mask,
                   model = list(D ~ slope_z, g0 ~ h2, sigma ~ h2), 
                   detectfn = "hazard rate")

# MLA area models ###############################
#Inside/outside MLA activity areas model ...............
DMLA <- secr.fit(ch, mask = mask,
                 model = list(D ~ MLA, g0 ~ h2, sigma ~ h2),
                 detectfn = "hazard rate")


# Distance to MLA activity areas model ...............
Dd.to.MLA <- secr.fit(ch, mask = mask,
                      model = list(D ~ d.to.MLA_z, g0 ~ h2, sigma ~ h2),
                      detectfn = "hazard rate")


# human areas models ###################################
# human activity areas model ......................
Dhumans <- secr.fit(ch,mask = mask,
                    model = list(D ~ d.to.humans_z, g0 ~ h2, sigma ~ h2),
                    detectfn = "hazard rate")

# Airport model ...............................
DAirport <- secr.fit(ch,mask = mask,
                     model = list(D ~ d.to.Airport_z, g0 ~ h2, sigma ~ h2),
                     detectfn = "hazard rate")

# Camp Tinian model ........................
DCampTinian <- secr.fit(ch,mask = mask,
                        model = list(D ~ d.to.CampTinian_z, g0 ~ h2, sigma ~ h2),
                        detectfn = "hazard rate")

# Dump model .......................................
DDump <- secr.fit(ch,mask = mask,
                  model = list(D ~ d.to.Dump_z, g0 ~ h2, sigma ~ h2),
                  detectfn = "hazard rate")

# North Field model.........................
DNorthField <- secr.fit(ch,
                        mask = mask,
                        model = list(D ~ d.to.NorthField_z, g0 ~ h2, sigma ~ h2),
                        detectfn = "hazard rate")

# Quarry model .......................................
DQuarry <- secr.fit(ch,mask = mask,
                    model = list(D ~ d.to.Quarry_z, g0 ~ h2, sigma ~ h2),
                    detectfn = "hazard rate")


# Town model ................................. 
DTown <- secr.fit(ch,mask = mask,model = list(D ~ d.to.Town_z, g0 ~ h2, sigma ~ h2),
                  detectfn = "hazard rate")


# VOA model ..............................
DVOA <- secr.fit(ch,mask = mask,
                 model = list(D ~ d.to.VOA_z, g0 ~ h2, sigma ~ h2),
                 detectfn = "hazard rate")

# compare AIC  ###########################
AIC(gsh2, Dhabitat, Dshore,Dshoresq, Droad, Dshore.road, Delev, Delevsq, Dslope, DMLA, Dd.to.MLA, Dhumans, 
    DAirport, DCampTinian,DDump, DNorthField, DQuarry, DTown, DVOA)

# 4. Save models ###################################
models <- list(
  gsh2 = gsh2,
  Dhabitat = Dhabitat,
  Dshore = Dshore,
  Dshoresq = Dshoresq,
  Droad = Droad,
  Dshore.road = Dshore.road,
  Delev = Delev,
  Delevsq = Delevsq,
  Dslope = Dslope,
  DMLA = DMLA,
  Dd.to.MLA = Dd.to.MLA,
  Dhumans = Dhumans,
  DAirport = DAirport,
  DCampTinian = DCampTinian,
  DDump = DDump,
  DNorthField = DNorthField,
  DQuarry = DQuarry,
  DTown = DTown,
  DVOA = DVOA
)

saveRDS(models, "secr_models.rds") # can save this out somewhere


# 5. Read in models if needed ###############################
models <- readRDS("secr_models.rds")

list2env(models, envir = .GlobalEnv)

# 6. Compare AIC ###################################
do.call(AIC, models)

AIC(gsh2, Dshore, Dhabitat)

AIC(gsh2, Dshore,Dshoresq, Droad, Dshore.road, Delev, Delevsq, Dslope, DMLA, Dd.to.MLA, Dhumans, 
    DAirport, cDCampTinian,DDump, DNorthField, DQuarry, DTown, DVOA)

# 7. Calculate abundance #################################
# Read in MLA boundary ................. 
MLA_boundary <- st_read(
  "C:/Users/celin/OneDrive/Desktop/Tinian_GIS_layers/MLA_boundary/MLA_Boundary_2025.shp"
)

MLA_boundary <- st_transform(MLA_boundary, 32655)

# read in island boundary
tinian <- st_read(
  "C:/Users/celin/OneDrive/Desktop/Tinian_GIS_layers/CNMI Hi-Res veg data/amidon_2016_tinian.shp"
)

# Standardize CRS metadata
st_crs(tinian) <- 32655

# Check
st_crs(tinian) == st_crs(MLA_boundary)

# dissolve polygons into single island boundary
island_boundary <- st_union(tinian)
island_poly <- vect(island_boundary)

# plot the MLA bondary
plot(island_poly)
plot(MLA_boundary, add = TRUE, border = "red", lwd = 2)

#extract MLA_mask coordinates from mask..............
mask_xy <- st_as_sf(
  as.data.frame(mask),
  coords = c("x", "y"),
  crs = st_crs(MLA_boundary)
)

inside <- lengths(st_within(mask_xy, MLA_boundary)) > 0

mask_MLA <- mask[inside, ]

covariates(mask_MLA) <- covariates(mask)[inside, ]

summary(mask_MLA)
head(covariates(mask_MLA))


# plot full mask
plot(mask, pch = 15, cex = 0.3)


# plot mask within MLA
plot(mask_MLA, pch = 15, cex = 0.3)


### within full mask  ##########################
region.N(
  Dshore,
  spacing = 250)

### only within MLA  ##########################
region.N(
  Dshore,
  region = mask_MLA,
  spacing = 250)
