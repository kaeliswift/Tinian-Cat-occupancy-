#===============================
# Spatial Capture-Recapture (Cats)
# Cleaned version (March 2026)
#===============================

library(secr)
library(tidyverse)
library(lubridate)
library(sf)

#-------------------------------
# 1. LOAD + CLEAN DATA
#-------------------------------

raw.data <- read.csv("TrapTagger_Cat_Individuals.csv")

data <- raw.data %>%
  filter(Individuals != "None") %>%
  mutate(Individuals = str_trim(Individuals)) %>%
  separate_rows(Individuals, sep = "\\|") %>%
  filter(Individuals != "unidentifiable") %>%
  mutate(DateTime = mdy_hm(Timestamp, tz = "Pacific/Guam"))

#-------------------------------
# 2. DEPLOYMENT FILTERING ~~~~MAY HAVE AN ISSUE HERE
#-------------------------------

start_end <- read.csv("Camera Depolyment and Termination.csv") %>%
  filter(Site.name != "") %>%
  rename(Site.Name = Site.name) %>%
  mutate(
    Deployment = mdy_hms(paste(Deployment, "00:00:00"), tz = "UTC"),
    Termination = mdy_hms(paste(Termination, "23:59:59"), tz = "UTC"))

#AVOIDS PROBLEM CHILDREN........... WILL NEED TO INSERT~~~~~~~~~~~~~~~~~~~~
start_end <- start_end %>%
  group_by(Site.Name) %>%
  slice(1) %>%
  ungroup()

# join + filter detections within active periods
data <- data %>%
  left_join(start_end, by = "Site.Name") %>%
  filter(DateTime >= Deployment & DateTime <= Termination)

#-------------------------------
# 3. DEFINE SESSIONS
#-------------------------------

data <- data %>%
  mutate(session = ifelse(year(DateTime) == 2024, 1, 2))

#-------------------------------
# 4. DEFINE OCCASIONS (FIXED)
#-------------------------------

data <- data %>%
  mutate(
    occasion = floor(as.numeric(difftime(DateTime, Deployment, units = "days")) / 7) + 1) %>%
  filter(occasion >= 1 & occasion <= 6)

#-------------------------------
# 5. COLLAPSE DETECTIONS
#-------------------------------

data <- data %>%
  group_by(session, Individuals, Site.Name, occasion) %>%
  summarise(DateTime = min(DateTime), .groups = "drop")

#-------------------------------
# 6. BUILD EFFORT MATRIX
#-------------------------------

start_end <- start_end %>%
  mutate(session = ifelse(year(Deployment) == 2024, 1, 2))

effort <- start_end %>%
  group_by(Site.Name, session) %>%
  do({
    df <- .
    data.frame(
      occasion = 1:6,
      active = sapply(1:6, function(o) {
        occ_start <- df$Deployment + days((o-1)*7)
        occ_end   <- occ_start + days(6)
        as.integer(df$Deployment <= occ_end & df$Termination >= occ_start)
      })
    )
  }) %>%
  ungroup()

effort.matrix <- effort %>%
  pivot_wider(names_from = occasion, values_from = active, values_fill = 0)

# split by session
effort_year1 <- effort.matrix %>%
  filter(session == 1) %>%
  arrange(Site.Name)

effort_year2 <- effort.matrix %>%
  filter(session == 2) %>%
  arrange(Site.Name)

#-------------------------------
# 7. TRAP FILE
#-------------------------------

trapfile <- read.csv("cat_cam_deployment_landcover_type.csv") %>%
  rename(
    TrapID = Label,
    x = Longitude,
    y = Latitude
  ) %>%
  mutate(TrapID = str_trim(TrapID))

# project coordinates
trap_sf <- st_as_sf(trapfile, coords = c("x", "y"), crs = 4326) %>%
  st_transform(crs = 32655)

coords <- st_coordinates(trap_sf)

trapfile_proj <- trapfile %>%
  mutate(
    x = coords[,1],
    y = coords[,2]
  ) %>%
  select(TrapID, x, y)

#-------------------------------
# 8. BUILD TRAPS (proper traps objects)
#-------------------------------

# For year1
trapfile_year1_df <- trapfile_proj %>%
  filter(TrapID %in% trapIDs_year1) %>%
  select(TrapID, x, y) %>%
  mutate(
    x = as.numeric(x),
    y = as.numeric(y)
  ) %>%
  as.data.frame()

# Set rownames to TrapID
rownames(trapfile_year1_df) <- trapfile_year1_df$TrapID
trapfile_year1_df$TrapID <- NULL

traps_year1 <- read.traps(trapfile_year1_df, detector = "proximity")

# For year2
trapfile_year2_df <- trapfile_proj %>%
  filter(TrapID %in% trapIDs_year2) %>%
  select(TrapID, x, y) %>%
  mutate(
    x = as.numeric(x),
    y = as.numeric(y)
  ) %>%
  as.data.frame()

rownames(trapfile_year2_df) <- trapfile_year2_df$TrapID
trapfile_year2_df$TrapID <- NULL

traps_year2 <- read.traps(trapfile_year2_df, detector = "proximity")

#-------------------------------
# 9. BUILD MULTI-SESSION TRAPS
#-------------------------------

traps <- secr::ms(traps_year1, traps_year2)

#-------------------------------
# 10. CAPTURE HISTORY
#-------------------------------

data.clean <- data %>%
  rename(ID = Individuals, TrapID = Site.Name) %>%
  select(session, ID, occasion, TrapID) %>%
  mutate(
    session = as.integer(session),
    occasion = as.integer(occasion),
    ID = as.character(ID),
    TrapID = as.character(TrapID)
  )

# Make capture history using multi-session traps (effort already attached)
capthist <- make.capthist(
  captures = data.clean,
  traps = traps,
  fmt = "trapID"
)

summary(capthist)