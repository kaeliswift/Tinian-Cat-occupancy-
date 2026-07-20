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



# old models with separate sessions ########################################
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

mg0h2 <- readRDS("mg0h2.rds")
msigmah2 <- readRDS("msigmah2.rds")
mDshoresigmah2 <- readRDS("mDshoresigmah2.rds")

region.N(
  m0,
  spacing = 250) #35 and 21

region.N(
  mg0h2,
  spacing = 250) #41 and 24

region.N(
  msigmah2,
  spacing = 250) #83 and 49

region.N(
  mDshoresigmah2,
  spacing = 250) #90 and 54

region.N(
  mDshore,
  spacing = 250) #34 and 22


# make new - TENTATIVE mask ###########################
# you will have to download and direct R to your GIS layers (they are too big to store in the repo)
tinian <- st_read(
  "C:/Users/celin/OneDrive/Desktop/Tinian_GIS_layers/CNMI Hi-Res veg data/tinian_release.shp"
)

# make sure CRS matches traps
st_crs(tinian)

# dissolve polygons into single island boundary
island_boundary <- st_union(tinian)

# convert island_boundary to SpatVect
island_poly <- vect(island_boundary)

suggest.buffer(ch) #8000 is close to 7000 .... will keep the same for now

mask <- make.mask(
  traps(ch),
  type = "trapbuffer",
  buffer = 7000,
  spacing = 250, 
  poly = island_poly
)

plot(mask)
plot(masklist[[1]]) #same thing

# new models with combined sessions ("session 3") #########################
# c0 ########
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
AIC(c0) #1203.285

saveRDS(c0, file = "c0.rds")
c0 <- readRDS("c0.rds")

region.N(
  c0,
  spacing = 250) #47

# cg0h2 ##########
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
AIC(cg0h2) #1036.605

saveRDS(cg0h2, file = "cg0h2.rds")
cg0h2 <- readRDS("cg0h2.rds")

region.N(
  cg0h2,
  spacing = 250) #74

# csigmah2 ###########
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
AIC(csigmah2) #1011.239
AIC(c0, cg0h2, csigmah2) # best by weight of 1

saveRDS(csigmah2, file = "csigmah2.rds")
csigmah2 <- readRDS("csigmah2.rds")

region.N(
  csigmah2,
  spacing = 250) #110 

# rough d.to.shore model ####################
mask.shore <- masklist[[1]]
class(mask.shore)
verify(mask.shore)

summary(mask.shore) #d.to.shore is currently scaled --- BE CAREFUL

cDshore <- secr.fit(
  ch,
  mask = mask.shore,
  model = list(
    D ~ d.to.shore,
    g0 ~ 1,
    sigma ~ 1
  ),
  detectfn = "halfnormal"
)

summary(cDshore)
AIC(cDshore, c0) #1175.454

saveRDS(cDshore, file = "cDshore.rds")
cDshore <- readRDS("cDshore.rds")

region.N(
  cDshore,
  spacing = 250) #50


# rough shore + sigma ~ h2 model ######################
#sigma ~ h2, D ~ d.to.shore
cDshoresigmah2 <- secr.fit(
  ch,
  mask = mask.shore,
  model = list(
    D ~ d.to.shore,
    g0 ~ 1,
    sigma ~ h2
  ),
  detectfn = "halfnormal"
)

summary(cDshoresigmah2)

saveRDS(cDshoresigmah2, "cDshoresigmah2.rds")

region.N(
  cDshoresigmah2,
  spacing = 250) #117

AIC(c0, cg0h2, csigmah2, cDshoresigmah2)

