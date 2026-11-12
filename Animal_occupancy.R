# Read in required packages ###########################
library(tidyverse) 
library(readxl) 
library(janitor) 
library(stringr)
library(hms)
library(lubridate)
library(ggplot2)
library(dplyr)
library(unmarked)
library(purrr)
library(sf)
library(terra)

# Step 1: Import the data ##############################
# 1A. Read in camera data................
# change the following based on your own path:
DataFull <- read_csv("C:\\Users\\celin\\OneDrive - UW\\kaelis's files - Tinian Forest Bird project\\Cat Occupancy Study\\Data\\CatOccupancy_ImageData.csv")
  
# format date and time in data full
DataFull$DateTime <- ymd_hms(DataFull$DateTime)

# fix sites w/ incorrect year
DataFull <- DataFull |>
  mutate(
    DateTime = case_when(
      Note0 == "Year is wrong" ~ `year<-`(DateTime, 2024),
      TRUE ~ DateTime
    )
  )

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

# Step 2: Format data for occupancy ################################
# 2A. Make occasion length 1 trap night.........
DataFull <- DataFull %>%
  mutate(Date = as.Date(DateTime))

# 2B. Define the species you'd like............
species <- "Cat"

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

# 2D. Create camera history........
active_days <- deployment_intervals |>
  mutate(
    Date = map2(start, end, ~ seq(.x, .y, by = "day"))
  ) |>
  unnest(Date) |>
  select(Site, Date) |>
  distinct() |>
  mutate(active = TRUE)

# 2E. Detections per trap night................
DataFull <- DataFull %>%
  mutate(detected = case_when(DataFull$Animal_1 == species |
                                Animal_2 == species ~ 1,
                                .default = 0))

# Create have 1 or 0 detections per site per day (binary)............
daily_det <- DataFull %>%
  ungroup() %>%
  group_by(Site, Date) %>%
  dplyr::summarise(det = max(detected), .groups = "drop")


# 2F. Create daily detection history................
all_dates <- seq(
  min(deployment_intervals$start),
  max(deployment_intervals$end),
  by = "day"
)

daily_history <- tidyr::expand_grid(
  Site = DeploymentData$Site,
  Date = all_dates
) %>%
  left_join(active_days, by = c("Site", "Date")) %>%
  left_join(daily_det, by = c("Site", "Date")) %>%
  mutate(
    active = replace_na(active, FALSE),
    
    det = case_when(
      !active ~ NA_integer_,
      is.na(det) ~ 0L,
      TRUE ~ as.integer(det)
    )
  )

daily_wide <- daily_history %>%
  select(Site, Date, det) %>%
  pivot_wider(
    names_from = Date,
    values_from = det
  ) %>%
  arrange(match(Site, DeploymentData$Site))

y_daily <- as.matrix(daily_wide %>% select(-Site))

rownames(y_daily) <- daily_wide$Site

dim(y_daily)
table(as.vector(y_daily), useNA = "ifany")

# 2G. Build weekly detection history......
weekly_history <- daily_history %>%
  dplyr::mutate(
    Week = lubridate::floor_date(Date, unit = "week", week_start = 1)
  ) %>%
  dplyr::group_by(Site, Week) %>%
  dplyr::summarise(
    days_active = sum(active, na.rm = TRUE),
    detections = sum(det, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  dplyr::mutate(
    det = dplyr::case_when(
      days_active == 0 ~ NA_integer_,
      detections > 0 ~ 1L,
      TRUE ~ 0L
    )
  )

weekly_wide <- weekly_history %>%
  select(Site, Week, det) %>%
  pivot_wider(
    names_from = Week,
    values_from = det
  ) %>%
  arrange(match(Site, DeploymentData$Site))

y_weekly <- as.matrix(weekly_wide %>% select(-Site))

rownames(y_weekly) <- weekly_wide$Site

# 2H. Build unmarked dataframes........
umf_daily <- unmarkedFrameOccu(
  y = y_daily)

umf_weekly <- unmarkedFrameOccu(
  y = y_weekly)

# Step 3: Format covariate data ########################################
# 3A. Read in trap coordinates........
traps <- read_xls("C:\\Users\\celin\\OneDrive - UW\\kaelis's files - Tinian Forest Bird project\\Cat Occupancy Study\\Data\\cat_cam_deployment_landcover_type.xls")
glimpse(traps)

camera_sites <- traps %>%
  dplyr::rename(
    Site = Label,
    habitat = `CLASS/landcover`
  ) %>%
  mutate(
    Site = as.character(Site),
    habitat = as.character(habitat)
  )

# Check that each site occurs once
nrow(camera_sites)
n_distinct(camera_sites$Site)

camera_pts <- st_as_sf(
  camera_sites,
  coords = c("Longitude", "Latitude"),
  crs = 4326
)

camera_pts <- st_transform(camera_pts, 32655)

st_crs(camera_pts)

# 3B. Read in covariate layers.........
#habitat
habitat <- st_read(
  "C:/Users/celin/OneDrive/Desktop/Tinian_GIS_layers/CNMI Hi-Res veg data/amidon_2016_tinian.shp")

st_crs(camera_pts) == st_crs(habitat)

#shoreline
shoreline <- st_read(
  "C:\\Users\\celin\\OneDrive\\Desktop\\Tinian_GIS_layers\\Tinian_shoreline\\Tinian_Shoreline.shp"
)

st_crs(camera_pts) == st_crs(shoreline)

#roads
roads <- st_read(
  "C:/Users/celin/OneDrive/Desktop/Tinian_GIS_layers/Tinian_roads/Roads Tinian.shp")

roads <- st_geometry(roads)
st_crs(camera_pts) == st_crs(roads)
roads <- st_transform(roads, st_crs(camera_pts))
st_crs(camera_pts) == st_crs(roads)

#MLA
MLA_activity <- st_read(
  "C:/Users/celin/OneDrive/Desktop/Tinian_GIS_layers/MLA_activity_areas/MLA_activity areas.shp")
st_crs(MLA_activity) == st_crs(camera_pts)

#elevation
elev <- rast("C:/Users/celin/OneDrive/Desktop/Tinian_GIS_layers/tinian_dem")
st_crs(elev) == st_crs(camera_pts) # bad
st_crs(elev) #makes sure to add EPSG
crs(elev) <- "EPSG:32655"
st_crs(elev) == st_crs(camera_pts) # good

#slope
slope <- terrain(
  elev,
  v = "slope",
  unit = "degrees",
  neighbors = 8
) #THIS WILL BE SLOW

# 3C. Extract covariates at cameras........
# Extract elevation and slope at camera locations
camera_vect <- terra::vect(camera_pts)

elev_values <- terra::extract(elev, camera_vect)
slope_values <- terra::extract(slope, camera_vect)

site_covs <- camera_sites %>%
  dplyr::select(Site, habitat) %>%
  mutate(
    elevation = elev_values[[2]],
    slope = slope_values[[2]]
  )

# Extract Abimon habitat data
camera_habitat <- st_join(
  camera_pts,
  habitat %>% dplyr::select(VegClass),
  join = st_intersects,
  left = TRUE
)
amidon_habitat <- camera_habitat %>%
  st_drop_geometry() %>%
  dplyr::select(Site, VegClass)
site_covs <- site_covs %>%
  dplyr::select(-habitat) %>%
  left_join(amidon_habitat, by = "Site") %>%
  rename(habitat = VegClass)

# 3D.Calculate distance covariates
#shoreline
shoreline_boundary <- st_boundary(st_union(shoreline))
shore_dist <- st_distance(
  camera_pts,
  shoreline_boundary)
site_covs$d_to_shore <- as.numeric(shore_dist)

#roads
roads_union <- st_union(roads)
road_dist <- st_distance(
  camera_pts,
  roads_union)
site_covs$d_to_road <- as.numeric(road_dist)

#MLA
MLA_union <- st_union(MLA_activity)
MLA_dist <- st_distance(
  camera_pts,
  MLA_union)
site_covs$d_to_MLA <- as.numeric(MLA_dist)

# Identify cameras located inside an MLA area
site_covs$inside_MLA <- lengths(
  st_intersects(camera_pts, MLA_union)
) > 0

# 3E. Realign sites with detection matrix.....
site_covs <- site_covs %>%
  slice(match(rownames(y_weekly), Site))

# 3F. Check correlation......
continuous_covs <- site_covs %>%
  dplyr::select(
    d_to_shore,
    d_to_road,
    d_to_MLA,
    elevation,
    slope
  )

cor_matrix <- cor(
  continuous_covs,
  use = "pairwise.complete.obs",
  method = "pearson"
)

round(cor_matrix, 2)

# 3G. Standardize distance covariates......
site_covs <- site_covs %>%
  mutate(
    d_to_shore_z = as.numeric(scale(d_to_shore)),
    d_to_road_z = as.numeric(scale(d_to_road)),
    d_to_MLA_z = as.numeric(scale(d_to_MLA)),
    elevation_z = as.numeric(scale(elevation)),
    slope_z = as.numeric(scale(slope))
  )

# 3H. Add to unmarked dataframes.....
umf_daily <- unmarkedFrameOccu(
  y = y_daily,
  siteCovs = site_covs)
umf_weekly <- unmarkedFrameOccu(
  y = y_weekly,
  siteCovs = site_covs)

# Step 4: Occupancy models ############################
# 4A. Null occupancy models............... 
m0_daily <- occu(
  ~1 ~1,
  data = umf_daily)

m0_weekly <- occu(
  ~1 ~1,
  data = umf_weekly)

summary(m0_daily) # AIC of 659.9342 
summary(m0_weekly)

# Daily estimates
psi_daily <- backTransform(m0_daily, type = "state")
p_daily <- backTransform(m0_daily, type = "det")

# Weekly estimates
psi_weekly <- backTransform(m0_weekly, type = "state")
p_weekly <- backTransform(m0_weekly, type = "det")

# Display estimates
psi_daily
p_daily

psi_weekly
p_weekly

# Log-likelihood
logLik(m0_daily)
logLik(m0_weekly)

# Sticking with daily for now........

# 4B. Occupancy covariate models............
site_covs #covariates available

# habitat
m_psi_habitat <- occu(~1 ~ habitat, data = umf_daily)
summary(m_psi_habitat) # AIC of 657.3569 

# elevation 
m_psi_elevation <- occu(~1 ~ elevation_z, data = umf_daily)
summary(m_psi_elevation) # AIC of 654.3943 

# slope 
m_psi_slope <- occu(~1 ~ slope_z, data = umf_daily)
summary(m_psi_slope) # AIC of 661.8121 

# distance to shore
m_psi_shore <- occu(~1 ~ d_to_shore_z, data = umf_daily)
summary(m_psi_shore) # AIC of 654.4146

# distance to road 
m_psi_road <- occu(~1 ~ d_to_road_z, data = umf_daily)
summary(m_psi_road) # AIC of 660.722

# distance to MLA area
m_psi_MLA <- occu(~1 ~ d_to_MLA_z, data = umf_daily)
summary(m_psi_MLA) # AIC of 660.1903

# inside or outside of MLA area
m_psi_inside_MLA <- occu(~1 ~ inside_MLA, data = umf_daily)
summary(m_psi_inside_MLA) # AIC of 659.176

# 4C. Detection covariate models.....
site_covs #covariates available

# habitat
m_p_habitat <- occu(~ habitat ~1, data = umf_daily)
summary(m_p_habitat) # AIC of 601.3157 but failed to coverge

# elevation 
m_p_elevation <- occu(~ elevation_z ~1, data = umf_daily)
summary(m_p_elevation) # AIC of 661.8047 

# slope 
m_p_slope <- occu(~ slope_z ~1, data = umf_daily)
summary(m_p_slope) # AIC of 658.4493 

# distance to shore
m_p_shore <- occu(~ d_to_shore_z ~1, data = umf_daily)
summary(m_p_shore) # AIC of 643.6059 

# distance to road 
m_p_road <- occu(~ d_to_road_z ~1, data = umf_daily)
summary(m_p_road) # AIC of 647.5906

# distance to MLA area
m_p_MLA <- occu(~ d_to_MLA_z ~1, data = umf_daily)
summary(m_p_MLA) # AIC of 660.9646 

# inside or outside of MLA area
m_p_inside_MLA <- occu(~ inside_MLA ~1, data = umf_daily)
summary(m_p_inside_MLA) # AIC of 661.9342 but failed to converge


# ~~~~~~~~ Occupancy asymptote check ############################
# Sites with complete observations in first 30 days
active_per_day <- colSums(!is.na(y_daily))

# Dates with at least 30 active cameras
which(active_per_day >= 30)

# Corresponding dates and number of active cameras
data.frame(
  Date = names(active_per_day),
  Active_cameras = as.numeric(active_per_day)
) |>
  filter(Active_cameras >= 30)

# Find the column indices for the start and end dates
start_col <- match("2024-11-01", colnames(y_daily))
end_col <- match("2024-11-16", colnames(y_daily))

# Select the date range
y_window <- y_daily[, start_col:end_col]

# Count missing observations per site
missing_per_site <- rowSums(is.na(y_window))

# Identify sites active throughout the entire window
complete_window_sites <- rownames(y_window)[missing_per_site == 0]

length(complete_window_sites)
complete_window_sites

y_window_complete <- y_window[complete_window_sites, ]

m_window <- occu(
  ~1 ~1,
  data = unmarkedFrameOccu(y = y_window_complete)
)

summary(m_window)


# occupancy vs sampling window...............
# Fit models using 1 through 30 days
day_results <- map_dfr(1:30, function(days) {
  
  y_subset <- y_window_complete[, 1:days, drop = FALSE]
  
  model <- occu(
    ~1 ~1,
    data = unmarkedFrameOccu(y = y_subset)
  )
  
  psi <- backTransform(model, type = "state")
  
  tibble(
    Sampling_Days = days,
    Cameras = nrow(y_subset),
    Occupancy = as.numeric(coef(psi)),
    SE = as.numeric(SE(psi))
  )
})

day_results

# Graph.....
ggplot(day_results, aes(x = Sampling_Days, y = Occupancy)) +
  geom_line() +
  geom_point(size = 2.5) +
  geom_errorbar(
    aes(
      ymin = pmax(0, Occupancy - 1.96 * SE),
      ymax = pmin(1, Occupancy + 1.96 * SE)
    ),
    width = 0.2,
    na.rm = TRUE
  ) +
  scale_x_continuous(breaks = 1:30) +
  scale_y_continuous(limits = c(0, 1)) +
  labs(
    title = "Occupancy Estimates by Sampling Duration",
    x = "Sampling Duration (Days)",
    y = "Estimated Occupancy Probability"
  ) +
  theme_minimal()


# occupancy vs. number of cameras.........
set.seed(123)

camera_sizes <- c(5, 10, 15, 20, 25, 30, 35, 42)

camera_results <- map_dfr(camera_sizes, function(n_cameras) {
  
  map_dfr(1:50, function(iteration) {
    
    selected_sites <- sample(
      rownames(y_window_complete),
      size = n_cameras
    )
    
    y_subset <- y_window_complete[selected_sites, ]
    
    model <- tryCatch(
      occu(
        ~1 ~1,
        data = unmarkedFrameOccu(y = y_subset)
      ),
      error = function(e) NULL
    )
    
    if (is.null(model)) {
      return(tibble(
        Cameras = n_cameras,
        Iteration = iteration,
        Occupancy = NA_real_
      ))
    }
    
    psi <- backTransform(model, type = "state")
    
    tibble(
      Cameras = n_cameras,
      Iteration = iteration,
      Occupancy = as.numeric(coef(psi))
    )
  })
})
camera_summary <- camera_results |>
  group_by(Cameras) |>
  summarise(
    Mean_Occupancy = mean(Occupancy, na.rm = TRUE),
    Lower = quantile(Occupancy, 0.10, na.rm = TRUE),
    Upper = quantile(Occupancy, 0.90, na.rm = TRUE),
    .groups = "drop"
  )

# Graph.............
ggplot(camera_summary,
       aes(x = Cameras, y = Mean_Occupancy)) +
  geom_ribbon(
    aes(ymin = Lower, ymax = Upper),
    alpha = 0.2
  ) +
  geom_line() +
  geom_point(size = 2) +
  scale_y_continuous(limits = c(0, 1)) +
  scale_x_continuous(breaks = camera_sizes) +
  labs(
    title = "Occupancy Estimates by Camera Sample Size",
    x = "Number of Cameras",
    y = "Mean Estimated Occupancy Probability"
  ) +
  theme_minimal()
