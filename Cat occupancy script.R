#This is the first step towards making a new repo. 

library(tidyverse) # used for dataframe man
library(readxl) # import excel
library(janitor) # clean column
library(stringr)
library(hms)
library(lubridate)
library(ggplot2)
library(dplyr)
library(plyr)
library(unmarked)
#install.packages("bit64")

#Step 1 import the data 
#KAELI'S mac path 
CatDataFull <- suppressWarnings(
  read_csv("/Users/kaeliswift/Library/CloudStorage/OneDrive-UW/Tinian Forest Bird project/Cat Occupancy Study/Data/CatOccupancy_ImageData.csv")) 

#fix date and time in cat data full
CatDataFull$DateTime <- ymd_hms(CatDataFull$DateTime)

#read in veg data
habitat <- suppressWarnings(
  read_xls("/Users/kaeliswift/Library/CloudStorage/OneDrive-UW/Tinian Forest Bird project/Cat Occupancy Study/Data/cat_cam_deployment_landcover_type.xls"))

#Rename habitat column (new name=old name) and site column 
habitat <- habitat %>% 
  dplyr::rename(`habitat` = `CLASS/landcover`, `Site` = `Label`)


#Join vine df and site info df 
CatDataFull <- CatDataFull %>%
        left_join(habitat %>% select(`Site`, `habitat`),
            by = "Site")


#####Base occupancy###
#Choose survey occasion length (1 trap night)
CatDataFull$Date <- as.Date(CatDataFull$DateTime)

#detection variable
CatDataFull <- CatDataFull %>%
  mutate(cat_detect = case_when(CatDataFull$Animal_1 == "Cat"~1,
                                .default = 0))

#Create one one per site per day (binary)
daily_det <- CatDataFull %>%
  ungroup() %>%
  group_by(Site, Date) %>%
  dplyr::summarise(det = max(cat_detect), .groups = "drop")

#needed to add dplyr to summarise above, because otherwise it was only returning 1  thing. 

#create detection history
det_hist <- daily_det %>%
  tidyr::pivot_wider(names_from = Date, values_from = det, values_fill = 0)

#Remove site code to create matrix 
site_names <- det_hist$Site
y <- as.matrix(det_hist[ , -1])

#Building umarked frame
umf <- unmarkedFrameOccu(y = y)

#fit basic occupancy model 
m0 <- occu(~1 ~1, data = umf)
summary(m0)

#check detections 
site_occ <- apply(y, 1, max)
table(site_occ)


####Habiat Covariate model######

site_covs <- CatHabitat %>%
  group_by(Site) %>%
  dplyr::summarise(habitat = first(habitat), .groups = "drop")

site_covs$habitat <- as.factor(site_covs$habitat)

umf1 <- unmarkedFrameOccu(
  y = y,
  siteCovs = site_covs
)

m_hab <- occu(~1 ~ habitat, data = umf1)
summary(m_hab)

#####trying weekly detections####. 
#This didn't work-Matrix reflects a lot more than 6 weeks and there are obviously missing detections 

library(dplyr)
library(lubridate)
library(tidyr)
library(unmarked)

CatDataFull <- CatDataFull %>%
  mutate(Occasion = floor_date(DateTime, "week"))

all_sites <- unique(CatDataFull$Site)
all_weeks <- seq(
  min(CatDataFull$Occasion),
  max(CatDataFull$Occasion),
  by = "week"
)

full_grid <- expand.grid(
  Site = all_sites,
  Occasion = all_weeks
)

weekly_det <- CatDataFull %>%
  group_by(Site, Occasion) %>%
  dplyr::summarise(det = as.integer(any(cat_detect == 1)), .groups = "drop")

weekly_det_full <- full_grid %>%
  left_join(weekly_det, by = c("Site", "Occasion")) %>%
  mutate(det = ifelse(is.na(det), 0, det))

det_hist1 <- weekly_det_full %>%
  pivot_wider(
    names_from = Occasion,
    values_from = det
  )