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
library(camtrapR)

#install.packages("bit64")

#Step 1 import the data 
#KAELI'S mac path 
#CatDataFull <- suppressWarnings(
#  read_csv("/Users/kaeliswift/Library/CloudStorage/OneDrive-UW/Tinian Forest Bird project/Cat Occupancy Study/Data/CatOccupancy_ImageData.csv")) 
#habitat <- suppressWarnings(
#  read_xls("/Users/kaeliswift/Library/CloudStorage/OneDrive-UW/Tinian Forest Bird project/Cat Occupancy Study/Data/cat_cam_deployment_landcover_type.xls"))


#read the data from github
CatDataFull <- read.csv("CatOccupancy_ImageData.csv")
#fix date and time in cat data full (BG:changed mdy_hms to mdy_hm)
CatDataFull$DateTime <- mdy_hm(CatDataFull$DateTime)

#read in veg data
habitat <- read.csv("cat_cam_deployment_landcover_type.csv")

###Just for now, create fake deployment info based on camera info

# 1. Create a date object
habitat$Date<- as.Date(habitat$Date, "%m/%d/%Y")

# 2. Add 60 days using the '+' operator
habitat$EndDate<- habitat$Date + 60

##use "Date" in the 
#Rename habitat column (new name=old name) and site column 
habitat <- habitat %>% 
  dplyr::rename(`habitat` = `CLASS.landcover`, `Site` = `Label`, 'StartDate' = 'Date') 

cameraOp <- cameraOperation(habitat, 
                                   stationCol = "Site", 
                                   setupCol = "StartDate", 
                                   retrievalCol = "EndDate", 
                                   hasProblems = FALSE,
                                   allCamsOn = FALSE, 
                                   camerasIndependent = FALSE,
                                   dateFormat = "%Y-%m-%d", 
                                   writecsv = TRUE, 
)




cats <- detectionHistory(recordTable       = CatDataFull,
                              camOp                = cameraOp,
                              stationCol           = "Site",
                              speciesCol           = "Animal_1",
                              recordDateTimeCol    = "DateTime",
                              species              = "Cat",
                              occasionLength       = 7,
                              day1                 = "station",
                              datesAsOccasionNames = FALSE,
                              includeEffort        = FALSE,
                              scaleEffort          = FALSE,
                              timeZone             = "Pacific/Guam"
)


#####Base occupancy###

umf<- unmarkedFrameOccu(y = cats$detection_history)

m0 <- occu(~ 1 ~1, umf)

summary(m0)



####Habiat Covariate model######

habitat$habitat <- as.factor(habitat$habitat)

umf1 <- unmarkedFrameOccu(
  y = cats$detection_history,
  siteCovs = data.frame(habitat=habitat$habitat)
)

m_hab <- occu(~1 ~ habitat, data = umf1)
summary(m_hab)

m_hab_det <- occu(~habitat ~ 1, data = umf1)
summary(m_hab_det)
