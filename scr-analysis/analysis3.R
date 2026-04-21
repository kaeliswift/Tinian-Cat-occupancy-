#Cleaned version of analysis2
#4/20/26

library(secr)
library(tidyverse)
library(lubridate)
library(sf)
library(sp)

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
  group_by(Cluster.ID, Individuals) %>%
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
data <- data.filtered

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

#Analysis ########################################################################

#Look at ch
summary(ch)

#run the null model with a half normal detection
cats.HN<-secr.fit(ch, buffer=3000)

cats.HN

#get D, g0, and sigma on the real line (back-transformed from the link function)
predict(cats.HN)

#calculate the confidence interval for a parameter
confint(cats.HN, "D") #possible session error???

#what buffer should I use?
detpar <-  list(g0 = 0.27, sigma = 217)  #info from cats.Hn
str(detpar)
#suggest.buffer(ch.traps, "halfnormal", detpar, 6)  #6 occasions
#ISSUE --> could it be sessions???
ch.ses1 <- subset(ch, sessions = 1)
suggest.buffer(traps(ch.ses1), "halfnormal", detpar, 6)  #6 occasions
#720 m seems small???
ch.ses2 <- subset(ch, sessions = 2)
suggest.buffer(traps(ch.ses2), "halfnormal", detpar, 6)  #6 occasions
#757 M also small???

#you can also look at some metrics of movement within the dataset
#dbar is the mean distance between consecutive capture locations, 
#pooled over individuals (e.g. Efford 2004). 
#MMDM (for ‘Mean Maximum Distance Moved’) is the average maximum distance \
#between detections of each individual 

dbar(ch) #2089 for session 1 & 821 for session 2
MMDM(ch, min.recapt = 1, full = FALSE)
#3536 for session 1 & 1599 for session 2

##now let's look at a few other detection functions
cats.HZ <- secr.fit(ch, buffer = 3000, detectfn = 1)
cats.HZ
cats.EX <- secr.fit(ch, buffer = 3000, detectfn = 2)
cats.EX

## plot fitted detection functions
xv <- seq(0,800,10)
plot(cats.EX, xval = xv, limits = FALSE, lty = 2)
plot(cats.HN, xval = xv, limits = FALSE, lty = 1, add = TRUE)
plot(cats.HZ, xval = xv, limits = FALSE, lty = 3, add = TRUE)

#compare with AIC
aic.tab=AIC(cats.HN, cats.HZ, cats.EX)
aic.tab #cats.HZ has lowest AIC

cats.HZ #sigma = 587 
#sigma x 3 = 1761 meters

esa.plot(cats.HZ)
?esa.plot #being phased out

predict(cats.EX)
predict(cats.HN)
predict(cats.HZ)


#changing buffer to 2000
HN.2000 <-secr.fit(ch, buffer=2000, detectfn = 0)
HN.2000
HZ.2000 <- secr.fit(ch, buffer = 2000, detectfn = 1)
HZ.2000
EX.2000 <- secr.fit(ch, buffer = 2000, detectfn = 2)
EX.2000

#compare AICs
AIC(HN.2000, HZ.2000, EX.2000) #buffer = 2000, worse AICs HZ is still the best model
AIC(cats.HN, cats.HZ, cats.EX) #buffer = 3000
#are these comparable? -> NO models w/ different 


#Attempting vegetation mask again...............................................
veg_sf <- st_read("C:/Users/celin/Tinian-Cat-occupancy-/scr-analysis/CNMI Hi-Res veg data/tinian_release.shp") %>%
  st_transform(crs = st_crs(traps.sf))
plot(veg_sf) #can skip this, just shows you avaialable data

landcover <- make.mask(ch.traps, buffer = 3000, spacing = 100)

vegmask <- addCovariates(
  object = landcover,
  spatialdata = veg_sf,
  columns = "CLASS"
)

vegmask <- shareFactorLevels(vegmask) #issue w/ NAs
verify(vegmask)

#Version 1: NAs labeled as NoVeg and no clipping to veg extent....................
# fix NA issue
covariates(vegmask) <- lapply(covariates(vegmask), function(df) {
  # convert to character first (simplest + safest)
  df$CLASS <- as.character(df$CLASS)
  
  # replace NA
  df$CLASS[is.na(df$CLASS)] <- "NoVegData"
  
  # convert back to factor
  df$CLASS <- factor(df$CLASS)
  
  return(df)
})

vegmask <- shareFactorLevels(vegmask)
verify(vegmask)


#Version 2: clipped to veg extent................................. 
veg_sp <- as(veg_sf, "Spatial")

vegext <- make.mask(
  ch.traps,
  buffer = 3000,
  spacing = 100,
  type = "polygon",
  poly = veg_sp
)

vegext <- addCovariates(
  object = vegext,
  spatialdata = veg_sf,
  columns = "CLASS"
)

vegext <- shareFactorLevels(vegext)
verify(vegext) #nice

#Trying out fits of models with vegmask and vegext................................
#Version 1: Veg mask ~~~~~~~~~
# Null model
fit1_null <- secr.fit(
  ch,
  mask = vegmask,
  model = list(D ~ 1, g0 ~ 1, sigma ~ 1),
  detectfn = 1 #HZ was model w/ lowest AIC earlier
)

# Habitat model (density varies by vegetation)
fit1_Dhab <- secr.fit(
  ch,
  mask = vegmask,
  model = list(D ~ CLASS, g0 ~ 1, sigma ~ 1),
  detectfn = 1
) #issue here

#checking covariates actually exist
covariates(vegmask) #exists BUT a lot of NAs

#forcing CLASS correction for NAs
covariates(vegmask) <- lapply(covariates(vegmask), function(df) {
  
  # extract and force to character
  cls <- as.character(df$CLASS)
  
  # replace ALL missing values
  cls[is.na(cls) | cls == "" | cls == "NA"] <- "NoVegData"
  
  # trim whitespace (important)
  cls <- trimws(cls)
  
  # rebuild factor cleanly
  df$CLASS <- factor(cls)
  
  return(df)
})

vegmask <- shareFactorLevels(vegmask)
verify(vegmask)

#check of CLASS
lapply(covariates(vegmask), function(df) {
  list(
    n_NA = sum(is.na(df$CLASS)),
    table = table(df$CLASS)
  )
}) #STILL A LOT OF NAs ---> MOVE ON TO vegext


#compare models 
AIC(fit1_null, fit1_Dhab) #won't work

#Version 2: Veg extent ~~~~~~~~~~~~
# Null model
fit2_null <- secr.fit(
  ch,
  mask = vegext,
  model = list(D ~ 1, g0 ~ 1, sigma ~ 1)
)

# Habitat model
fit2_Dhab <- secr.fit(
  ch,
  mask = vegext,
  model = list(D ~ CLASS, g0 ~ 1, sigma ~ 1)
)

#compare models
AIC(fit2_null, fit2_Dhab)


predict(fit2_Dhab)
predict(fit2_Dhab)$D

coef(fit2_Dhab)
summary(fit2_Dhab)


#TO DO 4/21/26:
# 1. check what poly = does
  #GOOD - instead of creating a circular buffer around traps, the mask is clipped to your habitat shapefile geometry.
  #buffer defines potential movement space
  #poly defines habitat-constrained state space

# 2. reclassify down to ~5 (focus on what is at cams)

# 3. make NA adjacent class type or mask/remove them
# 4. put cats that were only captured once back in!
# 5. plot density vs buffer size (500, 1000, 15000 etc.)
# 6. find distance from edge of island to camera trap array/create plot?


#Creating vegext with paired down CLASS............................................
#look at CLASS at cam trap locations
table(traps$CLASS.landcover) 
#includes: Casuarina Thicket, 
#Mixed Introduced Forest, 
#Other Shrub and Grass, 
#Leucaena Leucocephala (Tangantangan), 
#Native Limestone Forest

#covariates in vegext
#session 1
table(covariates(vegext)[[1]]$CLASS)
#session 2
table(covariates(vegext)[[2]]$CLASS)

#pairing down CLASS in vegext -----> DID NOT WORK ---> WILL TAKE A BREAK!!!
recode_CLASS <- function(x) {
  
  x <- trimws(as.character(x))
  
  x[is.na(x) | x == "" | x == "NA"] <- "NoVegData"
  
  x[x %in% c("Native Limestone Forest")] <- "NativeForest"
  x[x %in% c("Casuarina Thicket")] <- "IronwoodForest"
  x[x %in% c("Leucaena Leucocephala (Tangantangan)")] <- "Tangantangan"
  
  x[x %in% c("Mixed Introduced Forest",
             "Agroforest",
             "Agroforest -- Coconut")] <- "IntroducedForest"
  
  x[x %in% c("Other Shrub and Grass",
             "Cropland")] <- "OpenShrubGrass"
  
  x[x %in% c("Urban and Built-up",
             "Urban Vegetation")] <- "Urban"
  
  x[x %in% c("Strand",
             "Barren/Sandy Beach/Bare Rocks",
             "Wetland")] <- "Coastal"
  
  x
}

covariates(vegext) <- lapply(covariates(vegext), function(df) {
  df$CLASS <- recode_CLASS(df$CLASS)
  df
})

all_levels <- sort(unique(unlist(lapply(covariates(vegext), function(df) {
  unique(df$CLASS)
}))))

covariates(vegext) <- lapply(covariates(vegext), function(df) {
  df$CLASS <- factor(df$CLASS, levels = all_levels)
  df
})

lapply(covariates(vegext), function(df) table(df$CLASS))

unique(unlist(lapply(covariates(vegext), function(df) as.character(df$CLASS))))
vegext <- shareFactorLevels(vegext)
verify(vegext)

table(covariates(vegext)[[1]]$CLASS)
table(covariates(vegext)[[2]]$CLASS)

st_crs(veg_sf)
st_crs(traps.sf)
