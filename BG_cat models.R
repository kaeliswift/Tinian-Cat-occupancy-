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
ch <- readRDS("scr-analysis/ch.RDS")
verify(ch)  # should say: no errors found :-)

# mask (script to create this in pt2_secr_mask.R)
mask <- readRDS("scr-analysis/mask.rds")
verify(mask)

###2. Run h2 models

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


AIC(null, gh2, gsh2, sh2) #sh2 is best, move forward with it



#null models #############################
null <- secr.fit(ch,mask = mask,
                 model = list(D ~ 1,g0 ~ 1,sigma ~ 1),
                 detectfn = 0)

#null models #############################
nullhaz <- secr.fit(ch,mask = mask,
                 model = list(D ~ 1,g0 ~ 1,sigma ~ 1),
                 detectfn = 1)

#null models #############################
null <- secr.fit(ch,mask = mask,
                 model = list(D ~ 1,g0 ~ 1,sigma ~ 1),
                 detectfn = "halfnormal")




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

Dshore <- secr.fit(ch,mask = mask,
                   model = list(D ~ d.to.shore_z,g0 ~ 1,sigma ~ h2),
                   detectfn = "halfnormal")

Dshoresq <- secr.fit(ch,mask = mask,
                     model = list(D ~ d.to.shore_z+ I(d.to.shore_z^2),g0~ 1,sigma ~ h2),
                     detectfn = "halfnormal")


# nearest road model #################################

Droad <- secr.fit(ch,mask = mask,
  model = list(D ~ d.to.road_z, g0 ~ 1, sigma ~ h2),
  detectfn = "halfnormal")


# shore + road model ######################
Dshore.road <- secr.fit(ch,mask = mask,
              model = list(D ~ d.to.shore_z + d.to.road_z, g0 ~ 1, sigma ~ h2),
              detectfn = "halfnormal")
  
# elevation model ######################################
Delev <- secr.fit(ch,mask = mask,
                   model = list(D ~ elev_z, g0 ~ 1, sigma ~ h2),
                   detectfn = "halfnormal")

# elev^2 model...............
Delevsq <- secr.fit(ch,mask = mask,
                     model = list(D ~ elev_z + I(elev_z^2), g0 ~ 1, sigma ~ h2),
                     detectfn = "halfnormal")


# slope model ######################################
Dslope <- secr.fit(ch, mask = mask,
                    model = list(D ~ slope_z, g0 ~ 1, sigma ~ h2), 
                    detectfn = "halfnormal")

# MLA area models ###############################
#Inside/outside MLA activity areas model ...............
DMLA <- secr.fit(ch, mask = mask,
                  model = list(D ~ MLA, g0 ~ 1, sigma ~ h2),
                  detectfn = "halfnormal")


# Distance to MLA activity areas model ...............
Dd.to.MLA <- secr.fit(ch, mask = mask,
                       model = list(D ~ d.to.MLA_z, g0 ~ 1, sigma ~ h2),
                       detectfn = "halfnormal")


# human areas models ###################################
# human activity areas model ......................
Dhumans <- secr.fit(ch,mask = mask,
                     model = list(D ~ d.to.humans_z, g0 ~ 1, sigma ~ h2),
                     detectfn = "halfnormal")

# Airport model ...............................
DAirport <- secr.fit(ch,mask = mask,
                      model = list(D ~ d.to.Airport_z, g0 ~ 1, sigma ~ h2),
                      detectfn = "halfnormal")

# Camp Tinian model ........................
DCampTinian <- secr.fit(ch,mask = mask,
                         model = list(D ~ d.to.CampTinian_z, g0 ~ 1, sigma ~ h2),
                         detectfn = "halfnormal")

# Dump model .......................................
DDump <- secr.fit(ch,mask = mask,
                   model = list(D ~ d.to.Dump_z, g0 ~ 1, sigma ~ h2),
                   detectfn = "halfnormal")

# North Field model.........................
DNorthField <- secr.fit(ch,
                         mask = mask,
                         model = list(D ~ d.to.NorthField_z, g0 ~ 1, sigma ~ h2),
                         detectfn = "halfnormal")

# Quarry model .......................................
DQuarry <- secr.fit(ch,mask = mask,
                     model = list(D ~ d.to.Quarry_z, g0 ~ 1, sigma ~ h2),
                     detectfn = "halfnormal")


# Town model ................................. 
DTown <- secr.fit(ch,mask = mask,model = list(D ~ d.to.Town_z, g0 ~ 1, sigma ~ h2),
                   detectfn = "halfnormal")


# VOA model ..............................
DVOA <- secr.fit(ch,mask = mask,
                  model = list(D ~ d.to.VOA_z, g0 ~ 1, sigma ~ h2),
                  detectfn = "halfnormal")

AIC(sh2, Dshore,Dshoresq, Droad, Dshore.road, Delev, Delevsq, Dslope, DMLA, Dd.to.MLA, Dhumans, 
    DAirport, cDCampTinian,DDump, DNorthField, DQuarry, DTown, DVOA)


region.N(
  Dshore,
  spacing = 250)

region.N(
  Delev,
  spacing = 250)

