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

Step 1 import the data 
#KAELI'S mac path 
CatDataFull <- suppressWarnings(
  read_csv("/Users/kaeliswift/Library/CloudStorage/OneDrive-UW/Tinian Forest Bird project/Cat Occupancy Study/Data/CatOccupancy_ImageData.csv")) 

#fix date and time in cat data full
CatDataFull$DateTime <- ymd_hms(CatDataFull$DateTime)

write.csv(CatDataFull, "CatDataFull.csv", row.names = FALSE)

#read in veg data
CatSiteInfo <- suppressWarnings(
  read_xls("/Users/kaeliswift/Library/CloudStorage/OneDrive-UW/Tinian Forest Bird project/Cat Occupancy Study/Data/cat_cam_deployment_landcover_type.xls"))


#Rename Landcover type column (new name=old name) and site column 
CatSiteInfo <- CatSiteInfo %>% 
  dplyr::rename(`Habitat` = `CLASS/landcover`, `Site` = `Label`)


#Read in deployment data 
DeploymentDat <- suppressWarnings(
  read_xlsx("/Users/kaeliswift/Library/CloudStorage/OneDrive-UW/Tinian Forest Bird project/Cat Occupancy Study/Data/Camera Depolyment and Termination.xlsx"))

#rename site column
DeploymentDat <- DeploymentDat %>% 
  dplyr::rename(`Site` = `Site name`)

#Join CatSiteInfo and DeploymentDat
SiteFull <- CatSiteInfo %>%
  left_join(DeploymentDat %>% select(`Site`, `Deployment`, `Termination`),
            by = "Site")

#Write out joined site info file 
write.csv(SiteFull, "Site.csv", row.names = FALSE)

