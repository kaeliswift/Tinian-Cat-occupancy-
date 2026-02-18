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

#fix date and time
CatDataFull$DateTime <- ymd_hms(CatDataFull$DateTime)

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






