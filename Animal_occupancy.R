# Read in required packages ###########################
library(tidyverse) 
library(readxl) 
library(janitor) 
library(stringr)
library(hms)
library(lubridate)
library(ggplot2)
library(dplyr)
library(plyr)
library(unmarked)

# Step 1: Import the data ##############################
# 1A. Read in camera data................
# change the following based on your own path:
DataFull <- read_csv("C:\\Users\\celin\\OneDrive - UW\\kaelis's files - Tinian Forest Bird project\\Cat Occupancy Study\\Data\\CatOccupancy_ImageData.csv")
  
# format date and time in data full
DataFull$DateTime <- ymd_hms(DataFull$DateTime)

# fix sites w/ incorrect year
DataFull <- DataFull |>
  mutate(
    DateTime = if_else(
      Note0 == "Year is wrong",
      update(DateTime, year = 2024),
      DateTime))


# 1B. Read in veg data........
# change the following based on your own path:
habitat <- read_xls("C:\\Users\\celin\\OneDrive - UW\\kaelis's files - Tinian Forest Bird project\\Cat Occupancy Study\\Data\\cat_cam_deployment_landcover_type.xls")

# Rename habitat column (new name=old name) and site column 
habitat <- habitat %>% 
  dplyr::rename(`habitat` = `CLASS/landcover`, `Site` = `Label`)


# 1C. Join data full and habitat data....... 
DataFull <- DataFull %>%
  left_join(habitat %>% select(`Site`, `habitat`),
            by = "Site")

# 1D. Read in deployment data.......
# change the following based on your own path:
DeploymentData <- 
  read_xlsx("C:\\Users\\celin\\OneDrive - UW\\kaelis's files - Tinian Forest Bird project\\Cat Occupancy Study\\Data\\Camera Depolyment and Termination.xlsx")

# rename site column
DeploymentData <- DeploymentData %>% 
  dplyr::rename(`Site` = `Site name`)

# 1E. Join data full and deployment info........ 
DataFull <- DataFull %>%
  left_join(DeploymentData %>% select(`Site`, `Deployment`, `Termination_1`,`Redeployment`,`Termination_2`),
            by = "Site")

# 1F. Assessing images taken during final two weeks for J4,J6 and G41
# they failed prematurely and were redeployed 
DataFull <- DataFull %>%
  mutate(
    DateTime = ymd_hms(DateTime),          # or ymd(), mdy(), etc.
    Termination_1 = ymd(Termination_1)
  )

DataFull <- DataFull %>%
  mutate(
    TwoWeeksPreTerm1 = case_when(
      Site %in% c("J4", "J6", "G41") &
        DateTime >= (Termination_1 - days(14)) &
        DateTime <= Termination_1 ~ TRUE,
      TRUE ~ FALSE
    )
  )

ProblemPeriodImages <- DataFull %>%
  filter(TwoWeeksPreTerm1)

#Filtering to redeployment window 
RedeployImages <- DataFull %>%
  filter(
    Site %in% c("J4", "J6", "G41"),
    DateTime >= Redeployment,
    DateTime <= Termination_2
  )

LastPhotoPreTerm1 <- DataFull %>%
  mutate(
    DateTime = ymd_hms(DateTime),
    Termination_1 = ymd(Termination_1)
  ) %>%
  filter(
    Site %in% c("J4", "J6", "G41"),
    DateTime < Termination_1
  ) %>%
  group_by(Site) %>%
  dplyr::summarise(
    Termination_1 = first(Termination_1),
    LastPhotoBeforeTerm1 = as.Date(max(DateTime, na.rm = TRUE)),
    DaysDifference = as.numeric(Termination_1 - LastPhotoBeforeTerm1)
  )

# Step 2: Format for occupancy ################################
# 2A. Make occasion length 1 trap night.........
DataFull$Date <- as.Date(DataFull$DateTime)

# 2B. Define the species you'd like............
species <- "Cow"

# 2C. Define deployment intervals......
deployment_intervals <- bind_rows(
  DeploymentData |>
    transmute(
      Site,
      start = as.Date(Deployment),
      end = as.Date(Termination_1)
    ),
  
  DeploymentData |>
    transmute(
      Site,
      start = as.Date(Redeployment),
      end = as.Date(Termination_2)
    )
) |>
  filter(!is.na(start), !is.na(end))

deployment_intervals |>
  arrange(Site, start)

# 2D. Create daily detection history........

# STOPPED HERE XXXXXXXXXXXXXXXXXXXXXXXXXXX #############

# 2C. Detection per trap night................
DataFull <- DataFull %>%
  mutate(detected = case_when(DataFull$Animal_1 == species |
                                Animal_2 == species ~ 1,
                                .default = 0))

# 2D. Create have 1 or 0 detections per site per day (binary)............
daily_det <- DataFull %>%
  ungroup() %>%
  group_by(Site, Date) %>%
  dplyr::summarise(det = max(detected), .groups = "drop")


# 2E. Create detection history................
det_hist <- daily_det %>%
  tidyr::pivot_wider(names_from = Date, values_from = det, values_fill = 0)

# 2F. Remove site code to create matrix......... 
site_names <- det_hist$Site
y <- as.matrix(det_hist[ , -1])

# 2G. Build unmarked dataframe........
umf <- unmarkedFrameOccu(y = y)

# Step 3: Occupancy models ############################
# 3A. Null occupancy model............... 
m0 <- occu(~1 ~1, data = umf)
summary(m0)

# 3B. Check how many sites the species is detected at....... 
site_occ <- apply(y, 1, max)
table(site_occ)


# 3C. Add habitat data (site-level covariate).........
site_covs <- habitat %>%
  group_by(Site) %>%
  dplyr::summarise(habitat = first(habitat), .groups = "drop")

site_covs$habitat <- as.factor(site_covs$habitat)

umf <- unmarkedFrameOccu(
  y = y,
  siteCovs = site_covs)

# 3D. Habitat occupancy model............
m_habitat <- occu(~1 ~ habitat, data = umf)
summary(m_habitat)
AIC(m0, m_habitat)

