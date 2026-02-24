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


#Step 1 import the data 
#read the data from github
ImageData <- read.csv("CatImageData.csv")

#reformat date
ImageData$DateTime <- as.POSIXct(ImageData$DateTime,
                                 format = "%Y-%m-%d %H:%M:%S",
                                 tz = "Pacific/Guam")

#check that it worked (should say POSIXct)
class(ImageData$DateTime)
head(ImageData$DateTime)


#read in site and deployment data
SiteFull <- read.csv("Site.csv")


#Create a camera operation file - camtrapR does this by assuming the camera is operational
#from the 'setup' to 'retrieval', but we can change that if there are any issues

SiteFull$cameraID <- as.numeric(ave(
  SiteFull$Site,
  SiteFull$Site,
  FUN = seq_along
))


cameraOp <- cameraOperation(
  SiteFull,
  stationCol = "Site",
  cameraCol = "cameraID",
  setupCol = "Deployment",
  retrievalCol = "Termination",
  allCamsOn = FALSE,
  byCamera = FALSE,
  camerasIndependent = FALSE,
  dateFormat = "%Y-%m-%d"
)


##Once the camera file is set up, now you set up the detection histories for any species
##and using "occasionLength" to say how long an occassion should be, I set it to 7 right now
##there were no cats listed in "Animal_2", so I'm just using the first animal column

ImageData_clean <- ImageData[!is.na(ImageData$Animal_1), ]

cats <- detectionHistory(recordTable       = ImageData_clean,
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
                              timeZone             = "Pacific/Guam",
)


####Habitat Covariate model######

# Create a data frame with one row per deployment
siteCovs_expanded <- SiteFull[match(rownames(cats$detection_history), SiteFull$Site), ]

siteCovs_expanded$Habitat[siteCovs_expanded$Habitat == "Casuarina Thicket"] <- "Mixed Introduced Forest"
siteCovs_expanded$Habitat[siteCovs_expanded$Habitat == "Native Limestone Forest"] <- "Mixed Introduced Forest"

# Keep only the covariate(s) you need
siteCovs_expanded <- data.frame(habitat = siteCovs_expanded$Habitat)

umf1 <- unmarkedFrameOccu(
  y = cats$detection_history,
  siteCovs = siteCovs_expanded
)

#####Base occupancy in unmarked###

m0 <- occu(~ 1 ~1, umf1)

m_hab <- occu(~1 ~ habitat, data = umf1)
summary(m_hab)

m_hab_det <- occu(~habitat ~ 1, data = umf1)
summary(m_hab_det)

m_hab_both <- occu(~habitat ~ habitat, data = umf1)
summary(m_hab_both)

