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

#read the data from github
CatDataFull <- read.csv("CatOccupancy_ImageData.csv")
#fix date and time in cat data full (BG:changed mdy_hms to mdy_hm)
CatDataFull$DateTime <- mdy_hm(CatDataFull$DateTime)

#read in veg data
habitat <- read.csv("cat_cam_deployment_landcover_type.csv")

###Just for now, create fake deployment info - use the date on the habitat file
###as the start date and then 60 days later as the end

# 1. Create a date object
habitat$Date<- as.Date(habitat$Date, "%m/%d/%Y")

# 2. Add 60 days using the '+' operator to create "EndDate"
habitat$EndDate<- habitat$Date + 60

#Rename habitat column (new name=old name), site column, and date
habitat <- habitat %>% 
  dplyr::rename(`habitat` = `CLASS.landcover`, `Site` = `Label`, 'StartDate' = 'Date') 

#Create a camera operation file - camtrapR does this by assuming the camera is operational
#from the 'setup' to 'retrieval', but we can change that if there are any issues

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

##Once the camera file is set up, now you set up the detection histories for any species
##and using "occasionLength" to say how long an occassion should be, I set it to 7 right now
##there were no cats listed in "Animal_2", so I'm just using the first animal column

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


#####Base occupancy in unmarked###

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
