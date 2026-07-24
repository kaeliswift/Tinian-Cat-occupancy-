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
ch <- readRDS("m_ch.RDS")
verify(ch)  # should say: no errors found :-)

# mask (script to create this in pt2_secr_mask.R)
masklist <- readRDS("masklist.rds")
verify(masklist) #bad
verify(masklist[[1]]) #good
verify(masklist[[2]]) #good
# force secr to recognize multi-session mask
class(masklist) <- c("mask", "list")
verify(masklist) #good

# MAY NEED TO TOSS ###########################################
# null model
#m0 <- readRDS("m0.rds")

# D ~ covariate models
#mDhabitat <- readRDS("mDhabitat.rds")
#mDsession <- readRDS("mDsession.rds")
#mDshore <- readRDS("mDshore.rds")
#mDroad <- readRDS("mDroad.rds")
#mDMLA <- readRDS("mDMLA.rds")
#mDd.to.MLA <- readRDS("mDd.to.MLA.rds")
#mDshoresq <- readRDS("mDshoresq.rds")
#mDshorecubed <- readRDS("mDshorecubed.rds")
#mDshore.road <- readRDS("mDshore.road.rds")
#mDelev <- readRDS("mDelev.rds")
#mDslope <- readRDS("mDslope.rds") 
#mDelevsq <- readRDS("mDelevsq.rds")
#mDhumans <- readRDS("mDhumans.rds")
#mDAirport <- readRDS("mDAirport.rds")
#mDCampTinian <- readRDS("mDCampTinian.rds")
#mDDump <- readRDS("mDDump.rds")
#mDNF <- readRDS("mDNF.rds")
#mDQuarry <- readRDS("mDQuarry.rds")
#mDTown <- readRDS("mDTown.rds")
#mDVOA <- readRDS("mDVOA.rds")

# AIC comparison of loaded models
#AIC(m0, mDhabitat, mDsession, mDshore, mDroad, mDshore.road, mDd.to.MLA, 
#    mDshoresq, mDshorecubed, mDelev, mDelevsq, mDhumans, mDAirport,
#    mDCampTinian, mDDump, mDNF, mDQuarry, mDTown, mDVOA,
#    mDslope)

#summary(mDshore) # top model
#summary(m0) # null model

# g0 ~ covariate model 
#mg0habitat <- readRDS("mg0habitat.rds")

#summary(mg0habitat)
#AIC(m0, mDhabitat, mg0habitat, mDshore) #not comparable 
#AIC(m0, mg0habitat) 

# 2. Code used to create the models ###########################################
# DO NOT RUN THIS CODE UNLESS YOU NEED TO RERUN A MODEL #################

# 2a. null models #############################
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

# D ~ covariate models ########################################
# habitat model ################
table(covariates(masklist[[1]])$habitat)
table(covariates(masklist[[2]])$habitat) #largest value needs to be reference level -> tangantangan

mDhabitat <- secr.fit(
  ch,
  mask = masklist,
  model = list(
    D ~ habitat,
    g0 ~ 1,
    sigma ~ 1),
  detectfn = "halfnormal")

summary(mDhabitat) #SE for some still pretty large....
AIC(m0, mDhabitat) #habitat is better > 10 AIC

saveRDS(mDhabitat, file = "mDhabitat.rds")
mDhabitat <- readRDS("mDhabitat.rds")

# cDhabitat produced a variance calculation warning (NaN SEs for some habitat coefficients),
# indicating that the habitat effects were not well identified. However, this model received
# essentially no support relative to the top model (ΔAICc > 30), so there is no need
# to investigate the unstable parameter estimates further. 

# session model #################
mDsession <- secr.fit(
  ch,
  mask = masklist,
  model = list(
  D ~ session,
  g0 ~ 1,
  sigma ~ 1),
  detectfn = "halfnormal")

summary(mDsession)
AIC(m0, mDhabitat, mDsession) #worse than the null model

saveRDS(mDsession, file = "mDsession.rds")
mDsession <- readRDS("mDsession.rds")

# distance to shore model ##################
mDshore <- secr.fit(
  ch,
  mask = masklist,
  model = list(
  D ~ d.to.shore_z,
  g0 ~ 1,
  sigma ~ 1),
  detectfn = "halfnormal")

summary(mDshore)
AIC(m0, mDhabitat, mDsession, mDshore) #best model so far

region.N(m0)
region.N(mDshore)

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

# distance to shore ^2 model.............................
mDshoresq <- secr.fit(
  ch,
  mask = masklist,
  model = list(
  D ~ d.to.shore_z + I(d.to.shore_z^2),
  g0 ~ 1,
  sigma ~ 1),
  detectfn = "halfnormal")

summary(mDshoresq)
AIC(m0, mDhabitat, mDsession, mDshore, mDroad, mDMLA, mDd.to.MLA, mDshoresq)

saveRDS(mDshoresq, file = "mDshoresq.rds")
mDshoresq <- readRDS("mDshoresq.rds")

# Quadratic model failed to converge to a meaningful solution
# (nlm code 5; non-identifiable parameter estimates), so it was
# not considered further. The linear shoreline model was retained.


# nearest road model #################################
mDroad <- secr.fit(
  ch,
  mask = masklist,
  model = list(
  D ~ d.to.road_z,
  g0 ~ 1,
  sigma ~ 1),
  detectfn = "halfnormal")

summary(mDroad)
AIC(m0, mDhabitat, mDsession, mDshore, mDroad)

saveRDS(mDroad, file = "mDroad.rds")
mDroad <- readRDS("mDroad.rds")


# distance to shore + distance to road.................................
mDshore.road <- secr.fit(
  ch,
  mask = masklist,
  model = list(D ~ d.to.shore_z + d.to.road_z, g0 ~ 1, sigma ~ 1),
  detectfn = "halfnormal")

summary(mDshore.road)
AIC(m0, mDshore, mDroad, mDshore.road)

saveRDS(mDshore.road, file = "mDshore.road.rds")
mDshore.road <- readRDS("mDshore.road.rds")

# elevation model ######################################
mDelev <- secr.fit(ch,
                   mask = masklist,
                   model = list(D ~ elev_z, g0 ~ 1, sigma ~ 1),
                   detectfn = "halfnormal")

summary(mDelev)
AIC(m0, mDshore, mDroad, mDshore.road, mDelev)


saveRDS(mDelev, file = "mDelev.rds")
mDelev <- readRDS("mDelev.rds")

# elev^2 model...............
mDelevsq <- secr.fit(ch,
                     mask = masklist,
                     model = list(D ~ elev_z + I(elev_z^2), g0 ~ 1, sigma ~ 1),
                     detectfn = "halfnormal")

summary(mDelevsq)
AIC(m0, mDshore, mDroad, mDshore.road, mDelev, mDelevsq)


saveRDS(mDelevsq, file = "mDelevsq.rds")
mDelevsq <- readRDS("mDelevsq.rds")

# elev^3 model...............
mDelevcubed <- secr.fit(ch,
                        mask = masklist,
                        model = list(D ~ elev_z + I(elev_z^2) +I(elev_z^3), g0 ~ 1, sigma ~ 1),
                        detectfn = "halfnormal")

summary(mDelevcubed)
AIC(m0, mDshore, mDroad, mDshore.road, mDelev, mDelevsq, mDelevcubed)


saveRDS(mDelevcubed, file = "mDelevcubed.rds")
mDelevcubed <- readRDS("mDelevcubed.rds")

# slope model ######################################
mDslope <- secr.fit(ch,
                    mask = masklist,
                    model = list(D ~ slope_z, g0 ~ 1, sigma ~ 1), 
                    detectfn = "halfnormal")

summary(mDslope)
AIC(m0, mDshore, mDroad, mDshore.road, mDelev, mDelevsq, mDslope)


saveRDS(mDslope, file = "mDslope.rds")
mDslope <- readRDS("mDslope.rds") 

hist(covariates(mask)$slope_z)

# MLA area models ###############################
#Inside/outside MLA activity areas model ...............
mDMLA <- secr.fit(ch,
                  mask = masklist,
                  model = list(D ~ MLA, g0 ~ 1, sigma ~ 1),
                  detectfn = "halfnormal")

summary(mDMLA)
AIC(m0, mDshore, mDroad, mDshore.road, mDelev, mDelevsq, mDslope, mDMLA)


# worse AIC than null model, no longer considered

# Distance to MLA activity areas model ...............
mDd.to.MLA <- secr.fit(ch,
                       mask = masklist,
                       model = list(D ~ d.to.MLA_z, g0 ~ 1, sigma ~ 1),
                       detectfn = "halfnormal")

summary(mDd.to.MLA)
AIC(m0, mDshore, mDroad, mDshore.road, mDelev, mDelevsq, mDslope, mDd.to.MLA)

saveRDS(mDd.to.MLA, file = "mDd.to.MLA.rds")
mDd.to.MLA <- readRDS("mDd.to.MLA.rds")

# human areas models ###################################
# human activity areas model ......................
mDhumans <- secr.fit(ch,
                     mask = masklist,
                     model = list(D ~ d.to.humans_z, g0 ~ 1, sigma ~ 1),
                     detectfn = "halfnormal")

summary(mDhumans)
AIC(m0, mDshore, mDroad, mDshore.road, mDelev, mDelevsq, mDslope, mDd.to.MLA, mDhumans)


saveRDS(mDhumans, file = "mDhumans.rds")
mDhumans <- readRDS("mDhumans.rds")

# check what covariates are available........................... 
summary(covariates(mask))

# Airport model ...............................
mDAirport <- secr.fit(ch,
                      mask = masklist,
                      model = list(D ~ d.to.Airport_z, g0 ~ 1, sigma ~ 1),
                      detectfn = "halfnormal")

summary(mDAirport)
AIC(m0, mDshore, mDroad, mDshore.road, mDelev, mDelevsq, mDslope, 
    mDd.to.MLA, mDhumans, mDAirport)


saveRDS(mDAirport, file = "mDAirport.rds")
mDAirport <- readRDS("mDAirport.rds")

# Camp Tinian model ........................
mDCampTinian <- secr.fit(ch,
                         mask = masklist,
                         model = list(D ~ d.to.CampTinian_z, g0 ~ 1, sigma ~ 1),
                         detectfn = "halfnormal")

summary(mDCampTinian)
AIC(m0, mDshore, mDroad, mDshore.road, mDelev, mDelevsq, mDslope, 
    mDd.to.MLA, mDhumans, mDAirport, mDCampTinian)

saveRDS(mDCampTinian, file = "mDCampTinian.rds")
mDCampTinian <- readRDS("mDCampTinian.rds")

# Dump model .......................................
mDDump <- secr.fit(ch,
                   mask = masklist,
                   model = list(D ~ d.to.Dump_z, g0 ~ 1, sigma ~ 1),
                   detectfn = "halfnormal")

summary(mDDump)
AIC(m0, mDshore, mDroad, mDshore.road, mDelev, mDelevsq, mDslope, 
    mDd.to.MLA, mDhumans, mDAirport, mDCampTinian, mDDump)

saveRDS(mDDump, file = "mDDump.rds")
mDDump <- readRDS("mDDump.rds")

# North Field model.........................
mDNorthField <- secr.fit(ch,
                         mask = masklist,
                         model = list(D ~ d.to.NorthField_z, g0 ~ 1, sigma ~ 1),
                         detectfn = "halfnormal")

summary(mDNorthField)
AIC(m0, mDshore, mDroad, mDshore.road, mDelev, mDelevsq, mDslope, 
    mDd.to.MLA, mDhumans, mDAirport, mDCampTinian, mDDump, mDNorthField)

saveRDS(mDNorthField, file = "mDNorthField.rds")
mDNorthField <- readRDS("mDNorthField.rds")

# Quarry model .......................................
mDQuarry <- secr.fit(ch,
                     mask = masklist,
                     model = list(D ~ d.to.Quarry_z, g0 ~ 1, sigma ~ 1),
                     detectfn = "halfnormal")

summary(mDQuarry)
AIC(m0, mDshore, mDroad, mDshore.road, mDelev, mDelevsq, mDslope, 
    mDd.to.MLA, mDhumans, mDAirport, mDCampTinian, mDDump, mDNorthField,
    mDQuarry)

saveRDS(mDQuarry, file = "mDQuarry.rds")
mDQuarry <- readRDS("mDQuarry.rds")

# Town model ................................. 
mDTown <- secr.fit(ch,
                   mask = masklist,model = list(D ~ d.to.Town_z, g0 ~ 1, sigma ~ 1),
                   detectfn = "halfnormal")

summary(mDTown)
AIC(m0, mDshore, mDroad, mDshore.road, mDelev, mDelevsq, mDslope, 
    mDd.to.MLA, mDhumans, mDAirport, mDCampTinian, mDDump, mDNorthField,
    mDQuarry, mDTown)

saveRDS(mDTown, file = "mDTown.rds")
mDTown <- readRDS("mDTown.rds")

# VOA model ..............................
mDVOA <- secr.fit(ch,
                  mask = masklist,
                  model = list(D ~ d.to.VOA_z, g0 ~ 1, sigma ~ 1),
                  detectfn = "halfnormal")

summary(mDVOA)
AIC(m0, mDshore, mDroad, mDshore.road, mDelev, mDelevsq, mDslope, 
    mDd.to.MLA, mDhumans, mDAirport, mDCampTinian, mDDump, mDNorthField,
    mDQuarry, mDTown, mDVOA)

saveRDS(mDVOA, file = "mDVOA.rds")
mDVOA <- readRDS("mDVOA.rds")


# AIC comparison #######################
AIC(m0, mDsession, mDshore, mDroad, mDshore.road, mDelev, mDelevsq, mDslope, 
    mDd.to.MLA, mDhumans, mDAirport, mDCampTinian, mDDump, mDNorthField,
    mDQuarry, mDTown, mDVOA, mDhabitat)

# BEWARE d.to.MLA and humans have r = 1.00 -- NEED TO SELECT ONE


# 3. h2 mixture models ############
#g0............
mg0h2 <- secr.fit(
  ch,
  mask = masklist,
  model = list(
    D ~ 1,
    g0 ~ h2,
    sigma ~ 1
  ),
  detectfn = "halfnormal"
)

summary(mg0h2)
saveRDS(mg0h2, "mg0h2.rds")
mg0h2 <- readRDS("mg0h2.rds")

AIC(m0, mg0h2)

region.N(
  m0,
  spacing = 250)

region.N(
  mg0h2,
  spacing = 250)


# sigma ~ h2 ........

msigmah2 <- secr.fit(
  ch,
  mask = masklist,
  model = list(
    D ~ 1,
    g0 ~ 1,
    sigma ~ h2
  ),
  detectfn = "halfnormal"
)


summary(msigmah2)
saveRDS(msigmah2, "msigmah2.rds")
msigmah2 <- readRDS("msigmah2.rds")

AIC(m0, mg0h2, msigmah2)

region.N(
  msigmah2,
  spacing = 250)

region.N(
  m0,
  spacing = 250)

# 4. N within the MLA ########################################################
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


# Extract MLA masks for every session
mask_MLA_list <- lapply(seq_along(masklist), function(i) {
  
  # coordinates for this session mask
  mask_xy <- st_as_sf(
    as.data.frame(masklist[[i]]),
    coords = c("x", "y"),
    crs = st_crs(MLA_boundary)
  )
  
  # identify mask cells inside MLA
  inside <- lengths(st_within(mask_xy, MLA_boundary)) > 0
  
  # subset this session's mask
  mask_MLA <- masklist[[i]][inside, ]
  
  # retain covariates
  covariates(mask_MLA) <- covariates(masklist[[i]])[inside, ]
  
  mask_MLA
})

# Check both MLA masks
summary(mask_MLA_list[[1]])
summary(mask_MLA_list[[2]])

#N from top D model...................

#calculate abundance in MLA
region.N(
  mDelevsq,
  region = mask_MLA,
  spacing = 250)

# calculate abundance in study area
region.N(
  mDelevsq,
  spacing = 250)

# csigmah2 model...................

#calculate abundance in MLA
region.N(
  msigmah2,
  region = mask_MLA,
  spacing = 250)

# calculate abundance in study area
region.N(
  msigmah2,
  spacing = 250)

# 5. Useful functions #####################################
closedN(ch) #assumes closed population

suggest.buffer(mDelevsq) #~9000 m for both sessions
