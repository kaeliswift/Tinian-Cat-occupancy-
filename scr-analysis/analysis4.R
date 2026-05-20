#Cleaned version of analysis2 with models
#4/23/26

library(secr)
library(tidyverse)
library(lubridate)
library(sf)
library(sp)
library(ggplot2)


#Formatting of data ##############################################################
#Make captfile....................................................................
#Session | Animal | Occasion | TrapID

#Individual identification was preformed on TrapTagger
raw.data <- read.csv("TrapTagger_Cat_Individuals.csv")

#Data Cleaning ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#removing None & unidentifiable & separating | detections
data <- raw.data %>%
  filter(Individuals != "None") %>%
  mutate(Individuals = str_trim(Individuals)) %>%
  separate_rows(Individuals, sep = "\\|") %>%
  filter(Individuals != "unidentifiable") %>%
  mutate(DateTime = mdy_hm(Timestamp, tz = "Pacific/Guam"))

#finding detections within deploy & termination dates.......................
start_end <- read.csv("Camera Depolyment and Termination.csv") %>%
  filter(Site.name != "") %>%
  rename(Site.Name = Site.name)

#dealing w/ problem deploy/termination dates..................
#problem sites: J6, J4, G41
#force 1st end date of J4 as 11/15/24
start_end[7, "Deployment"] <- "11/02/2024"
#force 1st end date of J6 to 11/15
start_end[10, "Termination"] <- "11/15/2024"
#G41 good as is

#format datetime of deploy & terminations..........
start_end <- start_end %>% 
  mutate(Deployment = mdy_hms(paste(Deployment, "00:00:00"), tz = "Pacific/Guam"),
         Termination = mdy_hms(paste(Termination, "23:59:59"), tz = "Pacific/Guam"))

#step 1: extract 1st set of with deploy/termination period
problem_children <- start_end[c(8,11,19),]
first_set <- start_end[-c(8,11,19),]

data.first_set <- data %>%
  left_join(first_set, by = "Site.Name") %>%
  group_by(Site.Name) %>% 
  filter(DateTime >= Deployment & DateTime <= Termination) %>%
  distinct(File, .keep_all = TRUE)

#step 2: extract 2nd set of within deploy/termination period from problem children 
data.prob_child <- data %>%
  left_join(problem_children, by = "Site.Name") %>%
  group_by(Site.Name) %>% 
  filter(DateTime >= Deployment & DateTime <= Termination) %>%
  distinct(File, .keep_all = TRUE)

#step 3: join the two together
data.studyperiod <- rbind(data.first_set, data.prob_child)

nrow(data) - nrow(data.studyperiod) #17 accounts different

#check of problem children
J4 <- data.studyperiod %>%
  filter(Site.Name == "J4") %>%
  arrange(DateTime)

J6 <- data.studyperiod %>%
  filter(Site.Name == "J6") %>%
  arrange(DateTime) #looks  ok

#step 4: replacing data with detections w/in the study period
data <- data.studyperiod

#Defining Session~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
data <- data %>%
  mutate(Session = ifelse(year(DateTime) == 2024, 1, 2))

#Defining Animal~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
data <- data %>% 
  mutate(Animal = Individuals)

#Filtering for individual and unique cluster (grouping of detections >30 min apart)
#This will remove multiple detections for the same individual w/in a similar time period
data.clean <- data %>%
  arrange(Cluster.ID, Individuals, DateTime) %>%   
  group_by(Site.Name, Cluster.ID, Individuals) %>%
  slice(1) %>%                           
  ungroup()

#THIS CODE ALLOWS FOR MULTIPLE DETECTIONS OF THE SAME INDIVIDUAL PER OCCASION 
#BUT REMOVES DETECTIONS OF THE SAME INDIVIDUAL W/IN A 30-MIN PERIOD AT A SITE
#Sets it up for detector = "count"
#We may need to change this in the future!!!!!!!!!!

#replacing data w/ detections >30 min apart
data <- data.clean

#checking for only 1 encounter......
table(data.clean$Individuals)
length(unique(data.clean$Individuals)) #47 individuals

#REMOVING INDIVIDUALS W/ ONLY 1 ENCOUNTER
#counts <- table(data.clean$Individuals)
#data.filtered <- data.clean[data.clean$Individuals %in% names(counts[counts > 1]), ]
#length(unique(data.filtered$Individuals)) #26 individuals 

#replacing data w/ individuals with k > 1 encounters 
#data <- data.filtered

#Defining Occasion~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#chose 1 week = 1 occasion
#chose to cap at 6 occasions to match methods of 6-week deployment
#the above can be changed by modifying the below code: 

#previous version: uses camera start date as occasion start for each trap
#data <- data %>%
 # mutate(Occasion = #takes the date and calculates occasion relative to deployment date
   #        floor(as.numeric(difftime(DateTime, Deployment, units = "days")) / 7) + 1) %>%
  #filter(Occasion >= 1 & Occasion <= 6) #filters for only 6 occasions 

#NEW version: SESSION-WIDE OCCASIONS with the same start date per session

session_starts <- tibble(
  Session = c(1, 2),
  SessionStart = as.POSIXct(
    c("2024-10-17 00:00:00",
      "2025-04-24 00:00:00"),
    tz = "Pacific/Guam"
  )
)

data <- data %>%
  left_join(session_starts, by = "Session") %>%
  mutate(
    Occasion = floor(
      as.numeric(
        difftime(DateTime, SessionStart, units = "days")
      ) / 7
    ) + 1
  ) %>%
  filter(Occasion >= 1 & Occasion <= 6)


#Defining TrapID~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#make sure to check for white spaces / differences in spelling
table(data$Site.Name) #looks ok
length(unique(data$Site.Name)) #appropriate number of cams (48 had cats)

data <- data %>% 
  mutate(TrapID = Site.Name)


#Make trapfile...................................................................
#TrapID | X | Y | Effort | / | Covariates
#One trapfile per session

#Data Cleaning~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Defining TrapID~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
traps <- read.csv("cat_cam_deployment_landcover_type.csv") %>%
  rename(TrapID = Label,
         x = Longitude,
         y = Latitude) %>%
  mutate(TrapID = str_trim(TrapID)) #cleans for extra spaces in Label

#Defining X & Y~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#x & y are currently in long & lat 
#but need to be in meters for secr to work
#step 1: create a sf object
traps.sf <- st_as_sf(traps, coords = c("x", "y"), crs = 4326) %>%
  st_transform(crs = 32655)
#step 2: convert the coordinates
coords <- st_coordinates(traps.sf)
#step 3: replace the coordinates 
traps.proj <- traps %>%
  mutate(x = coords[,1],
         y = coords[,2]) %>%
  select(TrapID, x, y)

#step 4: create traps object to check conversion
traps.obj <- read.traps(
  data = traps.proj,
  detector = "count")
summary(traps.obj) #average spacing looks correct

#step 5: replace the coordinates in traps 
traps <- traps %>%
  mutate(x = coords[,1],
         y = coords[,2]) 

start_end <- start_end %>% 
  mutate(Session = ifelse(year(Deployment) == 2024, 1, 2))

occasions <- start_end %>%
  group_by(Site.Name, Session) %>%
  summarise(start_date = min(Deployment)) %>%
  ungroup()

#pulling trapIDs per sessions
trapIDs_year1 <- start_end %>%
  filter(Session == 1) %>%
  pull(Site.Name) %>%
  unique()

trapIDs_year2 <- start_end %>%
  filter(Session == 2) %>%
  pull(Site.Name) %>%
  unique()

#pulling traps info per session
traps_year1 <- traps %>%
  filter(TrapID %in% trapIDs_year1) %>%
  select(TrapID, x, y) 

traps_year2 <- traps %>%
  filter(TrapID %in% trapIDs_year2) %>%
  select(TrapID, x, y) 

#Creating txt files for making capt. hist. ....................................................................

capt <- data %>% select(Session, Animal, Occasion, TrapID)

#writing text files in format that read.capthist() will like
write.table(capt, "capt_all.txt",
            row.names = FALSE, col.names = FALSE,
            quote = FALSE, sep = "\t")

write.table(traps_year1, "traps_year1.txt",
            row.names = FALSE, col.names = FALSE,
            quote = FALSE, sep = "\t")

write.table(traps_year2, "traps_year2.txt",
            row.names = FALSE, col.names = FALSE,
            quote = FALSE, sep = "\t")

#Creating capture history.......................................................
ch <- read.capthist(
  captfile = "capt_all.txt",
  trapfile = list("traps_year1.txt", "traps_year2.txt"),
  detector = "count",
  fmt = "trapID",
  binary.usage = FALSE) #this one gave me a lot of issues....

summary(ch)

#Plot ch for each session
par(mar = c(1,1,3,1)) 
plot (ch, tracks = TRUE) 

#Creating trap object..............................................................
ch.traps <- traps(ch)

#Adding usage & effort information to ch...............................................

#usage for session 1

#usage for session 2

# CREATE USAGE MATRICES ====================================================

make_usage_matrix <- function(session_num, traps_df, deploy_df, n_occasions = 6) {
  
  # traps for this session
  trap_ids <- traps_df$TrapID
  
  # deployment info for this session
  deploy_sess <- deploy_df %>%
    filter(Session == session_num)
  
  # initialize matrix
  usage_mat <- matrix(
    0,
    nrow = length(trap_ids),
    ncol = n_occasions,
    dimnames = list(trap_ids, paste0("occ", 1:n_occasions))
  )
  
  # session start date
  session_start <- min(deploy_sess$Deployment)
  
  # loop through traps
  for(i in seq_along(trap_ids)) {
    
    trap <- trap_ids[i]
    
    trap_info <- deploy_sess %>%
      filter(Site.Name == trap)
    
    if(nrow(trap_info) == 0) next
    
    deploy_date <- trap_info$Deployment[1]
    term_date   <- trap_info$Termination[1]
    
    # loop through occasions
    for(k in 1:n_occasions) {
      
      occ_start <- session_start + days((k - 1) * 7)
      occ_end   <- occ_start + days(7)
      
      # overlap between detector activity and occasion
      overlap_start <- max(deploy_date, occ_start)
      overlap_end   <- min(term_date, occ_end)
      
      overlap_days <- as.numeric(
        difftime(overlap_end, overlap_start, units = "days")
      )
      
      # convert to proportional effort
      effort <- max(0, min(1, overlap_days / 7))
      
      usage_mat[i, k] <- round(effort, 3)
    }
  }
  
  return(usage_mat)
}

usage_year1 <- make_usage_matrix(
  session_num = 1,
  traps_df = traps_year1,
  deploy_df = start_end
)

usage_year2 <- make_usage_matrix(
  session_num = 2,
  traps_df = traps_year2,
  deploy_df = start_end
)


#Analysis ########################################################################

#Look at ch
summary(ch)

usage(traps(ch[[1]]))

#estimate of sigma HN to suggest buffer size
#buffer size is usually 4 sigma HN
RPSV(ch, CC = TRUE)
#session 1 = 1800 * 4 = 7200
#session 2 = 695 * 4 = 2780

# different estimate of buffer for HN
suggest.buffer(ch, detectfn = 'HN', RBtarget = 0.001)
# session 1 = 8110
# session 2 = 3150

#run the null model with a half normal detection
cats.HN<-secr.fit(ch, buffer=2000, trace = FALSE)

cats.HN

#get D, g0, and sigma on the real line (back-transformed from the link function)
predict(cats.HN)

#plot buffer width (m) x n/esa(buffer)ha
par(pty = "s",mar = c(4,4,2,2),mgp =c(2.5,0.8,0),las =1)
esaPlot(cats.HN,ylim =c(0,7))
abline(v=8000, col ="red",lty =2)

suggest.buffer(cats.HN) #both ~8000

#run the null model with a half normal detection
cats.HN<-secr.fit(ch, buffer=8000, trace = FALSE)

predict(cats.HN)

esaPlot(cats.HN)

#Things to do: 
# 1. clip mask to MLA or trap array not island boundary
      # a. double check that does not mess w/ analysis
# 2. is the mask working correctly?
      # a. try using raster data instead
# 3. 4 classes for habitat covariates
# 4. double check that individual resights are ok/no corrections needed
# 5. add effort to ch.traps

#To dos: #1 - 3............................
 #raster data for cats
  #lidar shapefile from cats 

tinian <- st_read("C:/Users/celin/Tinian-Cat-occupancy-/scr-analysis/CNMI Hi-Res veg data/tinian_release.shp") 
names(tinian)
str(tinian)
plot(tinian["CLASS"])

#try converting shp into a raster.......


#FIGURE OUT INDIVIDUALS ISSUE!!..................................................................
apply(ch[[1]], 1, sum) #there are currently a high # of resights for select individuals
#THIS MIGHT CONTRIBUTE TO ISSUES w/ MODEL..........................