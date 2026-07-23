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

# 1a. read-in old session models ##############################################

# null model
m0 <- readRDS("m0.rds")

# D ~ covariate models
mDhabitat <- readRDS("mDhabitat.rds")
mDsession <- readRDS("mDsession.rds")
mDshore <- readRDS("mDshore.rds")
mDroad <- readRDS("mDroad.rds")
mDMLA <- readRDS("mDMLA.rds")
mDd.to.MLA <- readRDS("mDd.to.MLA.rds")
mDshoresq <- readRDS("mDshoresq.rds")
mDshorecubed <- readRDS("mDshorecubed.rds")
mDshore.road <- readRDS("mDshore.road.rds")
mDelev <- readRDS("mDelev.rds")
mDslope <- readRDS("mDslope.rds") 
mDelevsq <- readRDS("mDelevsq.rds")
mDhumans <- readRDS("mDhumans.rds")
mDAirport <- readRDS("mDAirport.rds")
mDCampTinian <- readRDS("mDCampTinian.rds")
mDDump <- readRDS("mDDump.rds")
mDNF <- readRDS("mDNF.rds")
mDQuarry <- readRDS("mDQuarry.rds")
mDTown <- readRDS("mDTown.rds")
mDVOA <- readRDS("mDVOA.rds")

# AIC comparison of loaded models
AIC(m0, mDhabitat, mDsession, mDshore, mDroad, mDshore.road, mDd.to.MLA, 
    mDshoresq, mDshorecubed, mDelev, mDelevsq, mDhumans, mDAirport,
    mDCampTinian, mDDump, mDNF, mDQuarry, mDTown, mDVOA,
    mDslope)

summary(mDshore) # top model
summary(m0) # null model

# g0 ~ covariate model 
mg0habitat <- readRDS("mg0habitat.rds")

summary(mg0habitat)
AIC(m0, mDhabitat, mg0habitat, mDshore) #not comparable 
AIC(m0, mg0habitat) 

# 2. Code used to create the models ###########################################
# DO NOT RUN THIS CODE UNLESS YOU NEED TO RERUN A MODEL #################
# 2a. null models #############################
c0 <- secr.fit(
  ch,
  mask = mask,
  model = list(
    D ~ 1,
    g0 ~ 1,
    sigma ~ 1
  ),
  detectfn = "halfnormal"
)

summary(c0)
AIC(c0) 

saveRDS(c0, file = "c0.rds")
c0 <- readRDS("c0.rds")

# null model with hazard rate
c0HR <- secr.fit(
  ch,
  mask = mask,
  model = list(
    D ~ 1,
    g0 ~ 1,
    sigma ~ 1
  ),
  detectfn = 1 # hazard rate
)

summary(c0HR)
AIC(c0, c0HR) 

# null model with exponential
c0EX <- secr.fit(
  ch,
  mask = mask,
  model = list(
    D ~ 1,
    g0 ~ 1,
    sigma ~ 1
  ),
  detectfn = 2 #exponential
)

summary(c0EX)
AIC(c0, c0HR, c0EX) #HR is best preforming detection rate but using HN for now because its estimates look better

summary(c0HR)

region.N(c0)
# continuing on with half normal detection rate for now 

# 2b. D ~ covariate models ########################################
#habitat model (WARNING - fails early) ##########################
cDhabitat <- secr.fit(
  ch,
  mask = mask,
  model = list(
  D ~ habitat,
  g0 ~ 1,
  sigma ~ 1),
  detectfn = "halfnormal")

summary(cDhabitat) #Fails & SE for some pretty large....
AIC(c0, cDhabitat) 

saveRDS(cDhabitat, file = "cDhabitat.rds")
cDhabitat <- readRDS("cDhabitat.rds")

# cDhabitat produced a variance calculation warning (NaN SEs for some habitat coefficients),
# indicating that the habitat effects were not well identified. However, this model received
# essentially no support relative to the top model (ΔAICc > 30), so there is no need
# to investigate the unstable parameter estimates further. 

# The habitat model was not investigated further because it was poorly
# supported (ΔAICc > 30 relative to the top model). The variance calculation
# warning therefore has no effect on model selection or inference.

# add habitat to traps to check which traps are where....
trap_habitat <- addCovariates(traps(ch), mask)

table(covariates(trap_habitat)$habitat)

# count detections by habitat
trapcov <- covariates(trap_habitat)

capt <- as.data.frame(ch)

capt$habitat <- trapcov[capt$TrapID, "habitat"]

table(capt$habitat)


capt <- as.data.frame(ch)

trapinfo <- data.frame(
  TrapID = rownames(traps(ch)),
  habitat = covariates(trap_habitat)$habitat
)

capt2 <- merge(capt, trapinfo, by = "TrapID")

table(capt2$habitat)


# shore model ##################

cDshore <- secr.fit(
  ch,
  mask = mask,
  model = list(
  D ~ d.to.shore_z,
  g0 ~ 1,
  sigma ~ 1),
  detectfn = "halfnormal")

summary(cDshore)
AIC(c0, cDshore) #best model so far

region.N(c0)
region.N(cDshore)

saveRDS(cDshore, file = "cDshore.rds")
cDshore <- readRDS("cDshore.rds")

#try graphing
hold=predictDsurface(cDshore, mask = mask, se.D = FALSE, cl.D = FALSE, alpha =0.05)
plot(island_poly)
plot(hold, add = TRUE)  
plot(traps(ch),  add = TRUE, col = "red", pch = 16)

esaPlot(mDshore) # good
plot(mDshore)

hist(covariates(mask)$d.to.shore_z)

# distance to shore ^2 model.............................
cDshoresq <- secr.fit(
  ch,
  mask = mask,
  model = list(
  D ~ I(d.to.shore_z) + I(d.to.shore_z^2),
  g0 ~ 1,
  sigma ~ 1 ), 
  detectfn = "halfnormal")

summary(cDshoresq)

saveRDS(cDshoresq, file = "cDshoresq.rds")
cDshoresq <- readRDS("cDshoresq.rds")


# Quadratic model failed to converge to a meaningful solution
# (nlm code 5; non-identifiable parameter estimates), so it was
# not considered further. The linear shoreline model was retained.


# nearest road model #################################

cDroad <- secr.fit(
  ch,
  mask = mask,
  model = list(D ~ d.to.road_z, g0 ~ 1, sigma ~ 1),
  detectfn = "halfnormal")

summary(cDroad)
AIC(c0, cDshore, cDroad)

saveRDS(cDroad, file = "cDroad.rds")
cDroad <- readRDS("cDroad.rds")


# shore + road model ######################
cDshore.road <- secr.fit(
  ch,
  mask = mask,
  model = list(D ~ d.to.shore_z + d.to.road_z, g0 ~ 1, sigma ~ 1),
  detectfn = "halfnormal")

summary(cDshore.road)
AIC(c0, cDshore, cDroad, cDshore.road)

saveRDS(cDshore.road, file = "cDshore.road.rds")
cDshore.road <- readRDS("cDshore.road.rds")

# elevation model ######################################
cDelev <- secr.fit(ch,
                   mask = mask,
                   model = list(D ~ elev_z, g0 ~ 1, sigma ~ 1),
                   detectfn = "halfnormal")

summary(cDelev)
AIC(c0, cDshore, cDroad, cDshore.road, cDelev)


saveRDS(cDelev, file = "cDelev.rds")
cDelev <- readRDS("cDelev.rds")

# elev^2 model...............
cDelevsq <- secr.fit(ch,
                   mask = mask,
                   model = list(D ~ elev_z + I(elev_z^2), g0 ~ 1, sigma ~ 1),
                   detectfn = "halfnormal")

summary(cDelevsq)
AIC(c0, cDshore, cDroad, cDshore.road, cDelev, cDelevsq)


saveRDS(cDelevsq, file = "cDelevsq.rds")
cDelevsq <- readRDS("cDelevsq.rds")

# elev^3 model...............
cDelevcubed <- secr.fit(ch,
                     mask = mask,
                     model = list(D ~ elev_z + I(elev_z^2) +I(elev_z^3), g0 ~ 1, sigma ~ 1),
                     detectfn = "halfnormal")

summary(cDelevcubed)
AIC(c0, cDshore, cDroad, cDshore.road, cDelev, cDelevsq, cDelevcubed)


saveRDS(cDelevcubed, file = "cDelevcubed.rds")
cDelevcubed <- readRDS("cDelevcubed.rds")


# slope model ######################################
cDslope <- secr.fit(ch,
                   mask = mask,
                   model = list(D ~ slope_z, g0 ~ 1, sigma ~ 1), 
                   detectfn = "halfnormal")

summary(cDslope)
AIC(c0, cDshore, cDroad, cDshore.road, cDelev, cDelevsq, cDslope)


saveRDS(cDslope, file = "cDslope.rds")
cDslope <- readRDS("cDslope.rds") 

hist(covariates(mask)$slope_z)

# MLA area models ###############################
#Inside/outside MLA activity areas model ...............
cDMLA <- secr.fit(ch,
                  mask = mask,
                  model = list(D ~ MLA, g0 ~ 1, sigma ~ 1),
                  detectfn = "halfnormal")

summary(cDMLA)
AIC(c0, cDshore, cDroad, cDshore.road, cDelev, cDelevsq, cDslope, cDMLA)


# Quadratic model failed to converge to a meaningful solution
# (nlm code 5; non-identifiable parameter estimates), so it was
# not considered further. 

# Distance to MLA activity areas model ...............
cDd.to.MLA <- secr.fit(ch,
                       mask = mask,
                       model = list(D ~ d.to.MLA_z, g0 ~ 1, sigma ~ 1),
                       detectfn = "halfnormal")

summary(cDd.to.MLA)
AIC(c0, cDshore, cDroad, cDshore.road, cDelev, cDelevsq, cDslope, cDd.to.MLA)

saveRDS(cDd.to.MLA, file = "cDd.to.MLA.rds")
cDd.to.MLA <- readRDS("cDd.to.MLA.rds")

hist(covariates(mask)$d.to.MLA_z)

# human areas models ###################################
# human activity areas model ......................
cDhumans <- secr.fit(ch,
                     mask = mask,
                     model = list(D ~ d.to.humans_z, g0 ~ 1, sigma ~ 1),
                     detectfn = "halfnormal")

summary(cDhumans)
AIC(c0, cDshore, cDroad, cDshore.road, cDelev, cDelevsq, cDslope, cDd.to.MLA, cDhumans)


saveRDS(cDhumans, file = "cDhumans.rds")
cDhumans <- readRDS("cDhumans.rds")

# check what covariates are available........................... 
summary(covariates(mask))

# Airport model ...............................
cDAirport <- secr.fit(ch,
                      mask = mask,
                      model = list(D ~ d.to.Airport_z, g0 ~ 1, sigma ~ 1),
                      detectfn = "halfnormal")

summary(cDAirport)
AIC(c0, cDshore, cDroad, cDshore.road, cDelev, cDelevsq, cDslope, 
    cDd.to.MLA, cDhumans, cDAirport)


saveRDS(cDAirport, file = "cDAirport.rds")
cDAirport <- readRDS("cDAirport.rds")

# Camp Tinian model ........................
cDCampTinian <- secr.fit(ch,
                         mask = mask,
                         model = list(D ~ d.to.CampTinian_z, g0 ~ 1, sigma ~ 1),
                         detectfn = "halfnormal")

summary(cDCampTinian)
AIC(c0, cDshore, cDroad, cDshore.road, cDelev, cDelevsq, cDslope, 
    cDd.to.MLA, cDhumans, cDAirport, cDCampTinian)

saveRDS(cDCampTinian, file = "cDCampTinian.rds")
cDCampTinian <- readRDS("cDCampTinian.rds")

# Dump model .......................................
cDDump <- secr.fit(ch,
                   mask = mask,
                   model = list(D ~ d.to.Dump_z, g0 ~ 1, sigma ~ 1),
                   detectfn = "halfnormal")

summary(cDDump)
AIC(c0, cDshore, cDroad, cDshore.road, cDelev, cDelevsq, cDslope, 
    cDd.to.MLA, cDhumans, cDAirport, cDCampTinian, cDDump)

saveRDS(cDDump, file = "cDDump.rds")
cDDump <- readRDS("cDDump.rds")

# North Field model.........................
cDNorthField <- secr.fit(ch,
                         mask = mask,
                         model = list(D ~ d.to.NorthField_z, g0 ~ 1, sigma ~ 1),
                         detectfn = "halfnormal")

summary(cDNorthField)
AIC(c0, cDshore, cDroad, cDshore.road, cDelev, cDelevsq, cDslope, 
    cDd.to.MLA, cDhumans, cDAirport, cDCampTinian, cDDump, cDNorthField)

saveRDS(cDNorthField, file = "cDNorthField.rds")
cDNorthField <- readRDS("cDNorthField.rds")

# Quarry model .......................................
cDQuarry <- secr.fit(ch,
                     mask = mask,
                     model = list(D ~ d.to.Quarry_z, g0 ~ 1, sigma ~ 1),
                     detectfn = "halfnormal")

summary(cDQuarry)
AIC(c0, cDshore, cDroad, cDshore.road, cDelev, cDelevsq, cDslope, 
    cDd.to.MLA, cDhumans, cDAirport, cDCampTinian, cDDump, cDNorthField,
    cDQuarry)

saveRDS(cDQuarry, file = "cDQuarry.rds")
cDQuarry <- readRDS("cDQuarry.rds")

# Town model ................................. 
cDTown <- secr.fit(ch,
                   mask = mask,model = list(D ~ d.to.Town_z, g0 ~ 1, sigma ~ 1),
                   detectfn = "halfnormal")

summary(cDTown)
AIC(c0, cDshore, cDroad, cDshore.road, cDelev, cDelevsq, cDslope, 
    cDd.to.MLA, cDhumans, cDAirport, cDCampTinian, cDDump, cDNorthField,
    cDQuarry, cDTown)

saveRDS(cDTown, file = "cDTown.rds")
cDTown <- readRDS("cDTown.rds")

# VOA model ..............................
cDVOA <- secr.fit(ch,
                  mask = mask,
                  model = list(D ~ d.to.VOA_z, g0 ~ 1, sigma ~ 1),
                  detectfn = "halfnormal")

summary(cDVOA)
AIC(c0, cDshore, cDroad, cDshore.road, cDelev, cDelevsq, cDslope, 
    cDd.to.MLA, cDhumans, cDAirport, cDCampTinian, cDDump, cDNorthField,
    cDQuarry, cDTown, cDVOA)

saveRDS(cDVOA, file = "cDVOA.rds")
cDVOA <- readRDS("cDVOA.rds")


# AIC comparison #######################
AIC(c0, cDshore, cDroad, cDshore.road, cDelev, cDelevsq, cDslope, 
    cDd.to.MLA, cDhumans, cDAirport, cDCampTinian, cDDump, cDNorthField,
    cDQuarry, cDTown, cDVOA, cDhabitat)

# BEWARE d.to.MLA and humans have r = 1.00 -- NEED TO SELECT ONE

# 3. h2 mixture models ############
#g0............
cg0h2 <- secr.fit(
  ch,
  mask = mask,
  model = list(
    D ~ 1,
    g0 ~ h2,
    sigma ~ 1
  ),
  detectfn = "halfnormal"
)

summary(cg0h2)
saveRDS(cg0h2, "mg0h2.rds")
cg0h2 <- readRDS("cg0h2.rds")

AIC(c0, cg0h2)

region.N(
  c0,
  spacing = 250)

region.N(
  cg0h2,
  spacing = 250)


# sigma ~ h2 ........

csigmah2 <- secr.fit(
  ch,
  mask = mask,
  model = list(
    D ~ 1,
    g0 ~ 1,
    sigma ~ h2
  ),
  detectfn = "halfnormal"
)


summary(csigmah2)
saveRDS(csigmah2, "csigmah2.rds")
csigmah2 <- readRDS("csigmah2.rds")

AIC(c0, cg0h2, csigmah2)

region.N(
  csigmah2,
  spacing = 250)

#sigma ~ h2, D ~ d.to.MLA
cDMLAsigmah2 <- secr.fit(
  ch,
  mask = mask,
  model = list(
    D ~ d.to.MLA_z,
    g0 ~ 1,
    sigma ~ h2
  ),
  detectfn = "halfnormal"
)

summary(cDMLAsigmah2)
saveRDS(cDMLAsigmah2, "cDMLAsigmah2.rds")

region.N(
  cDMLAsigmah2,
  spacing = 250)

region.N(
  c0,
  spacing = 250)

AIC(c0, cg0h2, csigmah2, cDMLAsigmah2)

# 4. bk models ###############################################################
# bk checks if there is a animal X site learned response
# interpretation can get tricky though if detector = "count" for ch which it does

cbk <-  secr.fit(
  ch,
  mask = mask,
  model = list(
    D ~ 1,
    g0 ~ bk,
    sigma ~ 1                     
  ),
  detectfn = "halfnormal"
)

AIC(c0, cbk)    

cbksigma <-  secr.fit(
  ch,
  mask = mask,
  model = list(
    D ~ 1,
    g0 ~ 1,
    sigma ~ bk                     
  ),
  detectfn = "halfnormal"
)

AIC(c0, cbk, cbksigma) 


# 5. N within the MLA ########################################################
# Estimating abundance & avg density in MLA 
MLA_boundary <- st_read("C:\\Users\\celin\\OneDrive\\Desktop\\Tinian_GIS_layers\\MLA_boundary\\MLA_Boundary_2025.shp")

crs(MLA_boundary)

#reproject to UTM 55N
MLA_boundary <- st_transform(MLA_boundary, 32655)

#read in shapefile data
tinian <- st_read(
  "C:/Users/celin/OneDrive/Desktop/Tinian_GIS_layers/CNMI Hi-Res veg data/tinian_release.shp")
# make sure CRS matches traps
st_crs(tinian) 

# dissolve polygons into single island boundary
island_boundary <- st_union(tinian)
island_poly <- vect(island_boundary)

plot(island_poly)
plot(MLA_boundary, add = TRUE, border = "red", lwd = 2)


#extract MLA_mask coordinates from mask.....
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

#cDd.to.MLA model...................

#calculate abundance in MLA
region.N(
  cDd.to.MLA,
  region = mask_MLA,
  spacing = 250)

# calculate abundance in study area
region.N(
  cDd.to.MLA,
  spacing = 250)

# csigmah2 model...................

#calculate abundance in MLA
region.N(
  csigmah2,
  region = mask_MLA,
  spacing = 250)

# calculate abundance in study area
region.N(
  csigmah2,
  spacing = 250)

# 6. Useful functions #####################################
closedN(ch) #assumes closed population

suggest.buffer(mDshore) #~9000 m for both sessions



