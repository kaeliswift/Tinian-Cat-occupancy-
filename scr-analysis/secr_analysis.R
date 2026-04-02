#Spatial Capture Recapture Analysis of Tinian Cats.......................................
#Created April 2026

#See SCR Assumptions at end of code

#use install.packages() for packages not in your library
library(secr)
library(tidyverse)
library(lubridate)
library(sf)


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
  mutate(Deployment = mdy_hms(paste(Deployment, "00:00:00"), tz = "UTC"),
       Termination = mdy_hms(paste(Termination, "23:59:59"), tz = "UTC"))

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
  group_by(Cluster.ID, Individuals) %>%
  slice(1) %>%                           
  ungroup()

#THIS CODE ALLOWS FOR MULTIPLE DETECTIONS OF THE SAME INDIVIDUAL PER OCCASION 
#BUT REMOVES DETECTIONS OF THE SAME INDIVIDUAL W/IN A 30-MIN PERIOD AT A SITE
#Sets it up for detector = "count"
#We may need to change this in the future!!!!!!!!!!

#replacing data w/ detections >30 min apart
data <- data.clean

#Defining Occasion~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#chose 1 week = 1 occasion
#chose to cap at 6 occasions to match methods of 6-week deployment
#the above can be changed by modifying the below code: 
data <- data %>%
  mutate(Occasion = #takes the date and calculates occasion relative to deployment date
           floor(as.numeric(difftime(DateTime, Deployment, units = "days")) / 7) + 1) %>%
  filter(Occasion >= 1 & Occasion <= 6) #filters for only 6 occasions 

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

#Defining Effort~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Find if each detector recorded within an occasion 
#CURRENT METHOD DOES NOT WORK --> WILL NEED TO REFORMAT!!!


#Defining Covariates~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#LandCover
#LOOK FOR ELEVATIONS ????
#Distance from road//humans???

#Pulling one trapfile per session~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#adding sessions to start_end
start_end <- start_end %>% 
mutate(Session = ifelse(year(Deployment) == 2024, 1, 2))

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

#To do................
#1. create proper mask
#2. add covariates to mask 
#3. try multisession models
#4. create effort matrix & incorporate that somehow


#Creating trap object..............................................................
ch.traps <- traps(ch)

#Creating mask........................................................................
#THESE #s CAN CHANGE BASED ON WHAT WE WANT
ch.mask <- make.mask(ch.traps, buffer = 1000, #buffer of 1 km around the points to make grid
                     spacing = 500, #mask points at 500 m intervals w/in the grid
                     type = "trapbuffer")

#Creating models......................................................................
#Null model - Half Harzard Normal
m0 <- secr.fit(ch, detectfn = "HHN", mask = ch.mask,
               model = list(D ~ 1, lambda0 ~ 1, sigma ~ 1))
print(m0) #will show m0 results

#Null model - Exponential
m0ex <- secr.fit(ch, detectfn = "EX", mask = ch.mask,
                model = list(D ~ 1, g0 ~ 1, sigma ~ 1))

#Density varies by Session
ms <- secr.fit(ch, detectfn = "HHN", mask = ch.mask,
               model = list(D ~ Session, lambda0 ~ 1, sigma ~ 1))
print(ms)

#FIGURE OUT IF WE SHOULD USE HHN OR EX

#Comparing models...........................................................
AIC(m0,m0ex,ms) #m0a has lower AIC

#idea of density from pt
#if you looked @ cats captured where their center activity is

#Plotting models.....................................................................
#issues w/ adding labs & multiple models will need to revisit
plot(m0a)
#mtext("Distance (m)", side = 1, line = 2.5)
#mtext("Detection prob.", side = 2, line = 2.5)

plot(m0)
#title(xlab = "Distance (m)", ylab = "Detection prob.")

#Detection probability (p)...............................................................
#Unknown # of animals NOT seen
#An unknown proportion of total animals seen 
#D = N/A = C.hat/p*A
#Activity center (D)
#Count (C.hat)

#Creating a raw # individuals per trap graph.................................................
individuals <- data %>% 
  group_by(TrapID) %>% 
  summarise(n_individuals = n_distinct(Animal))

individuals <- individuals %>% 
  right_join(traps, by = "TrapID")

individuals <- individuals %>% 
  select(TrapID, n_individuals, CLASS.landcover) %>% 
  rename(LandCover = CLASS.landcover)

individuals$n_individuals[is.na(individuals$n_individuals)] <- 0

individuals <- start_end %>% 
  rename(TrapID = Site.Name) %>% 
  right_join(individuals) %>% 
  select(TrapID, n_individuals, LandCover, Session)

individuals <- individuals %>% 
  distinct()

individuals <- individuals %>%
  mutate(Session = recode(Session,
                          `1` = "2024",
                          `2` = "2025"))

#sort by # individuals (removed for now)
#individuals <- individuals %>%
 # mutate(TrapID = factor(TrapID, levels = TrapID[order(-n_individuals)]))

library(ggplot2)

ggplot(individuals, aes(x = TrapID, y = n_individuals, fill = LandCover)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_viridis_d()+
  labs(
    x = "Trap",
    y = "Number of Individuals",
    fill = "Landcover",
    title = "Individuals per Trap by Land Cover"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


#SCR ASSUMPTIONS##################################################################
#1. Population is closed to changes
#2, Accurate id of individuals
#3. Detection is never perfect (p is fn of dist. from D-activity center)
#4. Size of mask is big enough to ensure no animals from outside the cam array get captured
#5. Independence btw individuals (ignore dependent individuals, ex.- kittens)

#WILL NEED TO CHECK THAT KITTEN IS REMOVED FROM THE ANALYSIS!!!!

