#Individual cats spatial-mark-recapture analysis 

#updated March 2025

#install.packages("secr")
library(secr)
library(tidyverse)
library(lubridate)


#LOOK AT CAMERA CHECKS/DEPLOYMENT SHEET........................................
cam.data <- read.csv("Cat study Schedule and checks MASTER.csv", header = T)
cam.data <- cam.data %>% 
  filter(Site.name != "") 

#clean cat individual ids...................................................
raw.data <- read.csv("TrapTagger_Cat_Individuals.csv")
table(raw.data$Individuals) #look at # of repeats per individual

#remove none & unidentifiable 
raw.data <- raw.data %>% 
  filter(Individuals != "None")
#split multiple captures into individual ones
data <- raw.data %>%
  mutate(Individuals = str_trim(Individuals)) %>%
  separate_rows(Individuals, sep = "\\|")   
#remove unidentifiable individuals
data <- data %>%
  filter(Individuals != "unidentifiable")


#fmt = "XY": session, ID, occasion, trap......................................
#session = study area or time period
  #sessions are assumed independent 
      #so detection histories do not span across different sessions
#occasion = trapping interval where traps are active (day, night, etc.)
#ID = individual

#session = year (1, 2 for year 1 and year 2)
#occasion = week (1, 2, 3, 4, 5, 6 for 6 week deployment)

#format DateTime
data <- data %>% 
  mutate(DateTime = mdy_hm(Timestamp, tz = "Pacific/Guam"))

#deployment & termination dates
#eliminate detections per site outside of deployment & termination
start_end <- read.csv("Camera Depolyment and Termination.csv")
start_end <- start_end %>% 
  filter(Site.name != "")
start_end <- start_end %>% 
  rename(Site.Name = Site.name)
start_end %>%
  count(Site.Name)

#problem sites: J6, J4, G41
#force 1st end date of J4 as 11/15/24
start_end[7, "Deployment"] <- "11/02/2024"
#force 1st end date of J6 to 11/15
start_end[10, "Termination"] <- "11/15/2024"
#G41 good as is

#check for misc. spaces -> good
unique(start_end$Site.Name)
unique(data$Site.Name)


#format date
start_end <- start_end %>% 
  mutate(Deployment = mdy_hms(paste(Deployment, "00:00:00"), tz = "Pacific/Guam"),
    Termination = mdy_hms(paste(Termination, "23:59:59"), tz = "Pacific/Guam"))

#Dealing w/ different deploy/termination dates..................
#step 1: extract 1st set of deploy/terminations
problem_children <- start_end[c(8,11,19),]
first_set <- start_end[-c(8,11,19),]

data.start_end <- data %>%
  left_join(first_set, by = "Site.Name") %>%
  group_by(Site.Name) %>% 
  filter(DateTime >= Deployment & DateTime <= Termination) %>%
  distinct(File, .keep_all = TRUE)

#step 2: extract 2nd set of deploy/terminations from problem children
data.prob <- data %>%
  left_join(problem_children, by = "Site.Name") %>%
  group_by(Site.Name) %>% 
  filter(DateTime >= Deployment & DateTime <= Termination) %>%
  distinct(File, .keep_all = TRUE)

#step 3: join the two together
data.studyperiod <- rbind(data.start_end, data.prob)

nrow(data) - nrow(data.studyperiod) #17 accounts different

#check of problem sites
J4 <- data.studyperiod %>%
  filter(Site.Name == "J4") %>%
  arrange(DateTime)

J6 <- data.studyperiod %>%
  filter(Site.Name == "J6") %>%
  arrange(DateTime) #looks  ok


#Define study sessions.................................
data <- data.studyperiod %>%
  mutate(session = ifelse(year(DateTime) == 2024, "year1", "year2"))
table(data$session)
table(year(data$DateTime)) #match so we are good

#Define study occasions................................ 
data <- data %>%
  group_by(session) %>%
  mutate(start_date = min(Deployment),
    occasion = floor(as.numeric(difftime(DateTime, start_date, units = "days")) / 7) + 1) %>%
  ungroup()
table(data$occasion) #9 occasions now

more_than_six <- data %>%  
  filter(occasion > 6)
table(more_than_six$Site.Name) #many sites.... weird

#filtering to 6 occasions 
data <- data %>%
  filter(occasion <= 6)
table(data$occasion) #6 occasions now

#Collapsing to 1 detection of an individual per occasion per trap.........
data.uncollapsed <- data
data <- data %>%
  group_by(session, Individuals, Site.Name, occasion, Deployment) %>%
  summarise(DateTime = min(DateTime), .groups = "drop") 

#creating an effort matrix......
occasions <- data %>%
  group_by(session) %>%
  summarise(start_date = min(Deployment)) %>%
  ungroup()

occasions <- occasions %>%
  rowwise() %>%
  mutate(occ_list = list(data.frame(
      occasion = 1:6,
      occ_start = start_date + days((0:5)*7)))) %>%
  unnest(occ_list) %>%
  mutate(occ_end = occ_start + days(6))

start_end <- start_end %>%
  mutate(session = ifelse(year(Deployment) == 2024, "year1", "year2"))

effort <- start_end %>%
  left_join(occasions, by = "session")
effort <- effort[,-c(4,5)]

effort <- effort %>%
  mutate(active = ifelse(
    Deployment <= occ_end & Termination >= occ_start, 1, 0))

effort <- effort %>%
  group_by(Site.Name, session, occasion) %>%
  summarise(active = max(active), .groups = "drop")

effort.matrix <- effort %>%
  pivot_wider(
    names_from = occasion,
    values_from = active,
    values_fill = 0)

effort_year1 <- effort.matrix %>%
  filter(session == "year1") %>%
  arrange(Site.Name)

effort_year2 <- effort.matrix %>%
  filter(session == "year2") %>%
  arrange(Site.Name)

effort_year1 <- as.matrix(effort_year1[, -c(1,2)])
effort_year2 <- as.matrix(effort_year2[, -c(1,2)])


#creating trapfile 
#TrapID, X, Y, Effort, /, Site-level covariates
trapfile <- read.csv("cat_cam_deployment_landcover_type.csv")
  
trapfile <- trapfile %>% 
  rename(TrapID = Label, 
         x = Longitude,
         y = Latitude,
         LandCover = CLASS.landcover) %>% 
  select(TrapID, x, y, LandCover)

#adding session year
sessionID <- start_end %>% 
  rename(TrapID = Site.Name) %>% 
  distinct(TrapID, session)

#checking TrapID spacing issue
#check for white spaces
unique(trapfile$TrapID)
unique(sessionID$TrapID)

trapfile <- trapfile %>%
  mutate(TrapID = str_trim(TrapID))

unique(trapfile$TrapID) #good now

#creating trapIDs per session
trapIDs_year1 <- sessionID %>%
 filter(session == "year1") %>%
 pull(TrapID) %>%
 unique()

trapIDs_year2 <- sessionID %>%
 filter(session == "year2") %>%
 pull(TrapID) %>%
 unique()


#Building trap objects.............................................
#transform coordinates from decimal degrees to meters
library(sf)

trap_sf <- st_as_sf(trapfile, coords = c("x", "y"), crs = 4326)
trap_sf <- st_transform(trap_sf, crs = 32655)

coords <- st_coordinates(trap_sf)

trapfile_proj <- trapfile %>%
  mutate(x = coords[,1],
    y = coords[,2]) %>%
  select(TrapID, x, y)

#creating traps objects
traps <- read.traps(
  data = trapfile_proj,
  detector = "proximity")
summary(traps_year1)

traps_year1 <- read.traps(
  data = trapfile_proj %>% filter(TrapID %in% trapIDs_year1),
  detector = "proximity")
summary(traps_year1) #check that trap spacing is ~ 1000 m -> good

traps_year2 <- read.traps(
  data = trapfile_proj %>% filter(TrapID %in% trapIDs_year2),
  detector = "proximity")
summary(traps_year2) #check that trap spacing is ~ 1000 m -> good

#Attaching effort per site..................
usage(traps_year1) <- effort_year1
usage(traps_year2) <- effort_year2

#STOPPED HERE NEED TO IMPROVE~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Building capture histories.................
data.clean <- data %>%
  rename(ID = Individuals, 
         TrapID = Site.Name) %>% 
  select(session, ID, occasion, TrapID)

str(data.clean)

#restructure data
data.clean <- data.clean %>%
  mutate(session = ifelse(session == "year1", "1", "2"))
str(data.clean)

data.clean <- data.clean %>%
  mutate(session = as.integer(session),
         ID = as.character(ID),
         occasion = as.integer(occasion),
         TrapID   = as.character(TrapID))
str(data.clean)


#make capture history
capthist <- make.capthist(
  captures = data.clean,
  traps = traps,
  fmt = "trapID") #DID NOT WORK


#no cats at camera: G51 & J5!!!


