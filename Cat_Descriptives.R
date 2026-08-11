
library(tidyverse) # used for dataframe man
library(readxl) # import excel
library(janitor) # clean column
library(stringr)
library(hms)
library(lubridate)
library(ggplot2)
library(dplyr)
library(plyr)



#Step 1 import the data 
#KAELI'S mac path 
CatImages <- suppressWarnings(
  read.csv("/Users/kaeliswift/Library/CloudStorage/OneDrive-UW/Tinian Forest Bird project/Cat Occupancy Study/Data/CatOccupancy_ImageData.csv")) 

CatLocations<- suppressWarnings(
  read_excel("/Users/kaeliswift/Library/CloudStorage/OneDrive-UW/Tinian Forest Bird project/Cat Occupancy Study/Data/cat_cam_deployment_landcover_type.xls")) 

#CatImages <-read.csv("CatOccupancy_ImageData.csv")

#CatLocations<- read_excel("cat_cam_deployment_landcover_type.xls")


#####Post processing steps#####
#Format dates 
CatImages <- CatImages |>
  dplyr::mutate(
    DateTime = as.POSIXct(DateTime, format = "%Y-%m-%d %H:%M:%S"),
    Date = as.Date(DateTime),
    Time = format(DateTime, "%H:%M:%S")
  )

#Drop all non-cat detections 
CatImages <- CatImages |>
  filter(Animal_1 == "Cat")

#rename labels to site
CatLocations <- CatLocations %>% 
  dplyr::rename(Site = Label)


#Join cat images with location information 
CatImages <- CatImages %>%
  left_join(CatLocations %>% select(Site, Latitude, Longitude, `CLASS/landcover`),
            by = "Site")

#Generate unique cat events (>30min apart)
CatActivity_events <- CatImages |>
  arrange(`Site`, DateTime) |>
  group_by(`Site`) |>
  mutate(
    time_diff_min = as.numeric(difftime(DateTime, lag(DateTime), units = "mins")),
    new_event = if_else(is.na(time_diff_min) | time_diff_min >= 30, 1, 0),
    event_id = cumsum(new_event),
    event_date = as.Date(DateTime)
  ) |>
  ungroup()

#Summarize unique events 
site_event_summary <- CatActivity_events |>
  dplyr::group_by(Site) |>
  dplyr::summarise(
    unique_events = dplyr::n_distinct(event_id),
    .groups = "drop"
  )

#Plot independant activity by hour
# Get one row per unique cat event
CatActivity_hourly <- CatActivity_events |>
  group_by(Site, event_id, Latitude, Longitude) |>
  dplyr::summarise(
    event_datetime = min(DateTime),
    .groups = "drop"
  ) |>
  mutate(
    hour = hour(event_datetime)
  )

# Plot number of unique detections by hour
ggplot(CatActivity_hourly, aes(x = hour)) +
  geom_histogram(
    binwidth = 1,
    boundary = -0.5
  ) +
  scale_x_continuous(
    breaks = seq(0, 23, by = 3),
    labels = sprintf("%02d:00", seq(0, 23, by = 3))
  ) +
  labs(
    x = "Hour of day",
    y = "Number of unique cat detections",
    title = "Unique cat detections across the 24-hour day"
  ) +
  theme_classic()

#plot all activity by the hour

CatImages_hourly <- CatImages |>
  mutate(hour = hour(Time)) |>
  count(hour)

ggplot(CatImages_hourly, aes(x = hour, y = n)) +
  geom_col() +
  scale_x_continuous(
    breaks = seq(0, 23, by = 3),
    labels = sprintf("%02d:00", seq(0, 23, by = 3))
  ) +
  labs(
    x = "Hour of day",
    y = "Number of cat detections",
    title = "Cat detections by hour"
  ) +
  theme_classic()


#total unique activities 
CatActivity_site <- CatActivity_hourly |>
  group_by(Site, Latitude, Longitude) |>
  dplyr::summarise(
    total_unique_detections = n(),
    .groups = "drop"
  )


library(leaflet) #required R package

#plot map - street map
leaflet(CatActivity_site) %>% setView(lng=145.6289, lat=15.04, zoom = 12) %>% 
  addProviderTiles(providers$Esri.WorldStreetMap) %>% 
  addCircleMarkers(~Longitude, ~Latitude, radius = ~total_unique_detections/2)

# Plot map - satellite map
leaflet(CatActivity_site) %>% 
  setView(lng = 145.6289, lat = 15.04, zoom = 12) %>% 
  addProviderTiles(providers$Esri.WorldImagery) %>% 
  addCircleMarkers(
    ~Longitude, 
    ~Latitude, 
    radius = ~total_unique_detections / 2,
    color = "red",
    fillColor = "red",
    fillOpacity = 0.7
  )