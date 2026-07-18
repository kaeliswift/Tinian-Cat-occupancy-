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
verify(masklist)

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

summary(cDhabitat) #SE for some still pretty large....
AIC(c0, cDhabitat) #habitat is better > 10 AIC

saveRDS(cDhabitat, file = "cDhabitat.rds")
cDhabitat <- readRDS("cDhabitat.rds")


# distance to shore model ##################

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
cDshore <- readRDS("mDshore.rds")

#try graphing
hold=predictDsurface(cDshore, mask = mask, se.D = FALSE, cl.D = FALSE, alpha =0.05)
plot(island_poly)
plot(hold, add = TRUE)  
plot(traps(ch),  add = TRUE, col = "red", pch = 16)

esaPlot(mDshore) # good
plot(mDshore)

# WILL NEED TO UPDATE BELOW MODELS #############################################
# distance to shore ^2 model.............................
#check that they are scaled
summary(covariates(masklist[[1]])$d.to.shore)
summary(covariates(masklist[[2]])$d.to.shore)

#mDshoresq <- secr.fit(
#  ch,
#  mask = masklist,
#  model = list(
#    D ~ d.to.shore + I(d.to.shore^2),
#    g0 ~ 1,
#    sigma ~ 1
#  ),
#  detectfn = "halfnormal"
#)

summary(mDshoresq)
AIC(m0, mDhabitat, mDsession, mDshore, mDroad, mDMLA, mDd.to.MLA, mDshoresq)

#saveRDS(mDshoresq, file = "mDshoresq.rds")
mDshoresq <- readRDS("mDshoresq.rds")

# distance to shore ^3 model...................
summary(covariates(masklist[[1]])$d.to.shore)
summary(covariates(masklist[[2]])$d.to.shore)

#mDshorecubed <- secr.fit(
#  ch,
#  mask = masklist,
#  model = list(
#    D ~ d.to.shore + I(d.to.shore^2) + I(d.to.shore^3),
#    g0 ~ 1,
#    sigma ~ 1
#  ),
#  detectfn = "halfnormal"
#)

summary(mDshorecubed)
AIC(m0, mDhabitat, mDsession, mDshore, mDroad, mDMLA, mDd.to.MLA, mDshoresq, mDshorecubed)

#saveRDS(mDshorecubed, file = "mDshorecubed.rds")
mDshorecubed <- readRDS("mDshorecubed.rds")


# distance to nearest road model #################################
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

#mDroad <- secr.fit(
#  ch,
#  mask = masklist,
#  model = list(
#    D ~ d.to.road,
#    g0 ~ 1,
#    sigma ~ 1
#  ),
#  detectfn = "halfnormal"
#)

summary(mDroad)
AIC(m0, mDhabitat, mDsession, mDshore, mDroad)

#saveRDS(mDroad, file = "mDroad.rds")
mDroad <- readRDS("mDroad.rds")


# distance to shore + distance to road.................................
#check that they are scaled
summary(covariates(masklist[[1]])$d.to.shore)
summary(covariates(masklist[[2]])$d.to.shore)

summary(covariates(masklist[[1]])$d.to.road)
summary(covariates(masklist[[2]])$d.to.road)

#mDshore.road <- secr.fit(
#  ch,
#  mask = masklist,
#  model = list(
#    D ~ d.to.shore + d.to.road,
#    g0 ~ 1,
#    sigma ~ 1
#  ),
#  detectfn = "halfnormal"
#)

summary(mDshore.road)
AIC(m0, mDhabitat, mDsession, mDshore, mDroad, mDMLA, mDd.to.MLA, mDshoresq, mDshore.road, mDshorecubed)

#saveRDS(mDshore.road, file = "mDshore.road.rds")
mDshore.road <- readRDS("mDshore.road.rds")

# elevation model ######################################
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

#mDelev <- secr.fit(
#  ch,
#  mask = masklist,
#  model = list(
#    D ~ elev,
#    g0 ~ 1,
#    sigma ~ 1
#  ),
#  detectfn = "halfnormal"
#)

summary(mDelev)
AIC(m0, mDhabitat, mDsession, mDshore, mDroad, mDMLA, mDd.to.MLA, 
    mDshoresq, mDshorecubed, mDelev)

#saveRDS(mDelev, file = "mDelev.rds")
mDelev <- readRDS("mDelev.rds")

# elev^2 model...............
summary(covariates(masklist[[1]])$elev) 
summary(covariates(masklist[[2]])$elev)

#mDelevsq <- secr.fit(
#  ch,
#  mask = masklist,
#  model = list(
#    D ~ elev + I(elev^2),
#    g0 ~ 1,
#    sigma ~ 1
#  ),
#  detectfn = "halfnormal"
#)

summary(mDelevsq)
AIC(m0, mDhabitat, mDsession, mDshore, mDroad, mDMLA, mDd.to.MLA, 
    mDshoresq, mDshorecubed, mDelev, mDelevsq)

#saveRDS(mDelevsq, file = "mDelevsq.rds")
mDelevsq <- readRDS("mDelevsq.rds")

# slope model ######################################
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


#mDslope <- secr.fit(
#  ch,
#  mask = masklist,
#  model = list(
#    D ~ slope,
#    g0 ~ 1,
#    sigma ~ 1
#  ),
#  detectfn = "halfnormal"
#)

summary(mDslope)
AIC(m0, mDhabitat, mDsession, mDshore, mDroad, mDMLA, mDd.to.MLA, 
    mDshoresq, mDshorecubed, mDelev, mDslope)

#saveRDS(mDslope, file = "mDslope.rds")
mDslope <- readRDS("mDslope.rds") 


# MLA models ###############################
# Inside/outside MLA activity areas model ...............
#mDMLA <- secr.fit(
#  ch,
#  mask = masklist,
#  model = list(
#    D ~ MLA,
#    g0 ~ 1,
#    sigma ~ 1
#  ),
#  detectfn = "halfnormal"
#)

summary(mDMLA)
AIC(m0, mDhabitat, mDsession, mDshore, mDroad, mDMLA)

#saveRDS(mDMLA, file = "mDMLA.rds")
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

#mDd.to.MLA <- secr.fit(
#  ch,
#  mask = masklist,
#  model = list(
#    D ~ d.to.MLA,
#    g0 ~ 1,
#    sigma ~ 1
#  ),
#  detectfn = "halfnormal"
#)

summary(mDd.to.MLA)
AIC(m0, mDhabitat, mDsession, mDshore, mDroad, mDMLA, mDd.to.MLA)

#saveRDS(mDd.to.MLA, file = "mDd.to.MLA.rds")
mDd.to.MLA <- readRDS("mDd.to.MLA.rds")

# distance from human areas models ###################################
# human activity areas model ......................
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

#mDhumans <- secr.fit(
#  ch,
#  mask = masklist,
#  model = list(
#    D ~ d.to.humans,
#    g0 ~ 1,
#    sigma ~ 1
#  ),
#  detectfn = "halfnormal"
#)

summary(mDhumans)
AIC(m0, mDhabitat, mDsession, mDshore, mDroad, mDMLA, mDd.to.MLA, 
    mDshoresq, mDshorecubed, mDelev, mDhumans)

#saveRDS(mDhumans, file = "mDhumans.rds")
mDhumans <- readRDS("mDhumans.rds")

# check what covariates are available 
summary(covariates(masklist[[1]]))

# Airport model ...............................
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

#mDAirport <- secr.fit(
#  ch,
#  mask = masklist,
#  model = list(
#    D ~ d.to.Airport,
#    g0 ~ 1,
#    sigma ~ 1
#  ),
#  detectfn = "halfnormal"
#)

summary(mDAirport)
AIC(m0, mDhabitat, mDsession, mDshore, mDroad, mDMLA, mDd.to.MLA, 
    mDshoresq, mDshorecubed, mDelev, mDhumans, mDAirport)

#saveRDS(mDAirport, file = "mDAirport.rds")
mDAirport <- readRDS("mDAirport.rds")

# Camp Tinian model ........................
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

#mDCampTinian <- secr.fit(
#  ch,
#  mask = masklist,
#  model = list(
#    D ~ d.to.CampTinian,
#    g0 ~ 1,
#    sigma ~ 1
#  ),
#  detectfn = "halfnormal"
#)

summary(mDCampTinian)
AIC(m0, mDhabitat, mDsession, mDshore, mDroad, mDMLA, mDd.to.MLA, 
    mDshoresq, mDshorecubed, mDelev, mDhumans, mDAirport,
    mDCampTinian)

#saveRDS(mDCampTinian, file = "mDCampTinian.rds")
mDCampTinian <- readRDS("mDCampTinian.rds")

# Dump model .......................................
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

#mDDump <- secr.fit(
#  ch,
#  mask = masklist,
#  model = list(
#    D ~ d.to.Dump,
#    g0 ~ 1,
#    sigma ~ 1
#  ),
#  detectfn = "halfnormal"
#)

summary(mDDump)
AIC(m0, mDhabitat, mDsession, mDshore, mDroad, mDMLA, mDd.to.MLA, 
    mDshoresq, mDshorecubed, mDelev, mDhumans, mDAirport,
    mDCampTinian, mDDump)

#saveRDS(mDDump, file = "mDDump.rds")
mDDump <- readRDS("mDDump.rds")

# North Field model.........................
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

#mDNF <- secr.fit(
#  ch,
#  mask = masklist,
#  model = list(
#    D ~ d.to.NorthField,
#    g0 ~ 1,
#    sigma ~ 1
#  ),
#  detectfn = "halfnormal"
#)

summary(mDNF)
AIC(m0, mDhabitat, mDsession, mDshore, mDroad, mDMLA, mDd.to.MLA, 
    mDshoresq, mDshorecubed, mDelev, mDhumans, mDAirport,
    mDCampTinian, mDDump, mDNF)

#saveRDS(mDNF, file = "mDNF.rds")
mDNF <- readRDS("mDNF.rds")

# Quarry model .......................................
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

#mDQuarry <- secr.fit(
#  ch,
#  mask = masklist,
#  model = list(
#    D ~ d.to.Quarry,
#    g0 ~ 1,
#    sigma ~ 1
#  ),
#  detectfn = "halfnormal"
#)

summary(mDQuarry)
AIC(m0, mDhabitat, mDsession, mDshore, mDroad, mDMLA, mDd.to.MLA, 
    mDshoresq, mDshorecubed, mDelev, mDhumans, mDAirport,
    mDCampTinian, mDDump, mDNF, mDQuarry)

#saveRDS(mDQuarry, file = "mDQuarry.rds")
mDQuarry <- readRDS("mDQuarry.rds")

# Town model ................................. 
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

#mDTown <- secr.fit(
#  ch,
#  mask = masklist,
#  model = list(
#    D ~ d.to.Town,
#    g0 ~ 1,
#    sigma ~ 1
#  ),
#  detectfn = "halfnormal"
#)

summary(mDTown)
AIC(m0, mDhabitat, mDsession, mDshore, mDroad, mDMLA, mDd.to.MLA, 
    mDshoresq, mDshorecubed, mDelev, mDhumans, mDAirport,
    mDCampTinian, mDDump, mDNF, mDQuarry, mDTown)

#saveRDS(mDTown, file = "mDTown.rds")
mDTown <- readRDS("mDTown.rds")

# VOA model ..............................
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

#mDVOA <- secr.fit(
#  ch,
#  mask = masklist,
#  model = list(
#    D ~ d.to.VOA,
#    g0 ~ 1,
#    sigma ~ 1
#  ),
#  detectfn = "halfnormal"
#)

summary(mDVOA)
AIC(m0, mDhabitat, mDsession, mDshore, mDroad, mDMLA, mDd.to.MLA, 
    mDshoresq, mDshorecubed, mDelev, mDhumans, mDAirport,
    mDCampTinian, mDDump, mDNF, mDQuarry, mDTown, mDVOA,
    mDslope, mDelevsq)

#saveRDS(mDVOA, file = "mDVOA.rds")
mDVOA <- readRDS("mDVOA.rds")

# Final AIC comparison of desired models #######################
AIC(m0, mDhabitat, mDsession, mDshore, mDroad, mDshore.road, mDd.to.MLA, 
    mDshoresq, mDshorecubed, mDelev, mDelevsq, mDhumans, mDAirport,
    mDCampTinian, mDDump, mDNF, mDQuarry, mDTown, mDVOA,
    mDslope)

# g0 ~ models ########################################
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
#mg0habitat <- secr.fit(
#  ch,
#  mask = masklist,
#  model = list(
#    D ~ 1,
#    g0 ~ site_habitat,
#    sigma ~ 1
#  ),
#  detectfn = "halfnormal"
#)

summary(mg0habitat)
AIC(m0, mDhabitat, mDsession, mDshore) #not comparable 
AIC(m0, mg0habitat) #lower AIC but somehow still not comparable


#saveRDS(mg0habitat, file = "mg0habitat.rds")
mg0habitat <- readRDS("mg0habitat.rds")

# 25. Calculate N with top model ###############################################
# Estimating abundance & avg density in MLA 
MLA_boundary <- st_read("C:\\Users\\celin\\OneDrive\\Desktop\\Tinian_GIS_layers\\MLA_boundary\\MLA_Boundary_2025.shp")

crs(MLA_boundary)

#reproject to UTM 55N
MLA_boundary <- st_transform(MLA_boundary, 32655)

#read in shapefile data
tinian <- st_read(
  "C:/Users/celin/OneDrive/Desktop/Tinian_GIS_layers/CNMI Hi-Res veg data/tinian_release.shp"
)
# make sure CRS matches traps
st_crs(tinian)
# dissolve polygons into single island boundary
island_boundary <- st_union(tinian)
island_poly <- vect(island_boundary)

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

#calculate abundance in MLA
region.N(
  mDshore,
  region = mask_MLA,
  spacing = 250)

# calculate abundance in study area
region.N(
  mDshore,
  spacing = 250)

# Trial of different functions #################
closedN(ch)

suggest.buffer(mDshore) #~9000 m for both sessions

# trying to see if things change with session effects
mDshore.sess <- secr.fit(
  ch,
  mask = masklist,
  model = list(
  D ~ d.to.shore + session,
  g0 ~ 1,
  sigma ~ 1
  ),
  detectfn = "halfnormal"
)

summary(mDshore.sess)
saveRDS(mDshore.sess, "mDshore_sess.rds")
AIC(m0, mDshore, mDshore.sess)

mDshore.sess <- readRDS("mDshore_sess.rds")

#calculate abundance in MLA
region.N(
  mDshore.sess,
  region = mask_MLA,
  spacing = 250)

# calculate abundance in study area
region.N(
  mDshore.sess,
  spacing = 250)


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

AIC(m0, cg0h2)

region.N(
  c0,
  spacing = 250)

region.N(
  cg0h2,
  spacing = 250)

cg0h2 <- readRDS("cg0h2.rds")

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
AIC(c0, cg0h2, csigmah2)

csigmah2 <- readRDS("csigmah2.rds")

region.N(
  csigmah2,
  spacing = 250)

#sigma ~ h2, D ~ d.to.shore
cDshoresigmah2 <- secr.fit(
  ch,
  mask = mask,
  model = list(
    D ~ d.to.shore_z,
    g0 ~ 1,
    sigma ~ h2
  ),
  detectfn = "halfnormal"
)

summary(cDshoresigmah2)
saveRDS(cDshoresigmah2, "cDshoresigmah2.rds")

region.N(
  cDshoresigmah2,
  spacing = 250)

AIC(c0, cg0h2, csigmah2, cDshoresigmah2)

