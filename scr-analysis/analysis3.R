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

#Checking D estimates & changes in buffer size ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#OK... next moves before models......
#graph buffer sizes & estimates
#decide on final buffer

#Exploring buffer sizes
buffers <- seq(500, 4000, by = 500) #checking 500 m to 4 km 

fits <- lapply(buffers, function(b) { #THIS WILL TAKE A LOT OF TIME 
  
  mask_b <- make.mask(
    traps(ch),
    buffer = b,
    spacing = 100,
    type = "polygon",
    poly = veg_sp
  )
  
  secr.fit(
    ch,
    mask = mask_b,
    detectfn = 1   # hazard rate
  )
})

saveRDS(fits, file = "D_buffer_fits.rds")
fits <- readRDS("D_buffer_fits.rds")


#extract density values (D)
D_values <- sapply(fits, function(fit) {
  sapply(derived(fit), function(x) x["D","estimate"])
})

#plot
library(tidyverse)

D_df <- as.data.frame(t(D_values))
colnames(D_df) <- c("Session1", "Session2")

D_df$buffer <- buffers

D_long <- D_df %>%
  pivot_longer(cols = starts_with("Session"),
               names_to = "Session",
               values_to = "D")

ggplot(D_long, aes(x = buffer, y = D, color = Session)) +
  geom_line() +
  geom_point() +
  labs(title = "Density vs Buffer Size",
       x = "Buffer (m)",
       y = "Density (D)") +
  theme_minimal()

ggplot(D_long, aes(x = buffer, y = D)) +
  geom_line() +
  geom_point() +
  facet_wrap(~Session) +
  labs(title = "Density vs Buffer Size",
       x = "Buffer (m)",
       y = "Density (D)") +
  theme_minimal()

#separate session plots: PAY CLOSE ATTENTION TO Y-VALUES
ggplot(subset(D_long, Session == "Session1"),
       aes(x = buffer, y = D)) +
  geom_line() +
  geom_point() +
  labs(title = "Session 1: Density vs Buffer Size",
       x = "Buffer (m)",
       y = "Density (D)") +
  theme_minimal()

ggplot(subset(D_long, Session == "Session2"),
       aes(x = buffer, y = D)) +
  geom_line() +
  geom_point() +
  labs(title = "Session 2: Density vs Buffer Size",
       x = "Buffer (m)",
       y = "Density (D)") +
  theme_minimal()

#fits <- lapply(buffers, function(b) {
# secr.fit(ch, buffer = b)}) #fitting all the buffers

#checking if D estimate stabilizes

#session 1
sapply(fits, function(fit) {
  derived(fit)[[1]]["D","estimate"]})
#session 2 
sapply(fits, function(fit) {
  derived(fit)[[2]]["D","estimate"]})

#both sessions 
sapply(fits, function(fit) {sapply(derived(fit), function(x) x["D","estimate"])})

#graphing
D_values <- sapply(fits, function(fit) {
  sapply(derived(fit), function(x) x["D","estimate"])
})

matplot(buffers, t(D_values), type = "b", pch = 1:2, col = 1:2,
        xlab = "Buffer (m)", ylab = "D estimate", lty = 1:2)
legend("topright", legend=c("Session 1","Session 2"), pch=1:2, lty=1:2)

#Creating vegetation mask that will clip mask to edge of island ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
veg_sf <- st_read("C:/Users/celin/Tinian-Cat-occupancy-/scr-analysis/CNMI Hi-Res veg data/tinian_release.shp") %>%
  st_transform(crs = st_crs(traps.sf))
plot(veg_sf) #can skip this, just shows you available data

#Version 2: clipped to veg extent................................. 
veg_sp <- as(veg_sf, "Spatial")

vegext <- make.mask(
  ch.traps,
  buffer = 3000,
  spacing = 100,
  type = "polygon",
  poly = veg_sp  #this uses the boundaries of veg to clip the mask
)

vegext <- addCovariates(
  object = vegext,
  spatialdata = veg_sf,
  columns = "CLASS"
)

vegext <- shareFactorLevels(vegext)
verify(vegext) #nice

#covariates in vegext
#session 1
table(covariates(vegext)[[1]]$CLASS)
#session 2
table(covariates(vegext)[[2]]$CLASS) #TOO MANY --> NEED TO PAIR DOWN

#Creating vegext with paired down CLASS............................................
#look at CLASS at cam trap locations
table(traps$CLASS.landcover) 
#includes: Casuarina Thicket, 
#Mixed Introduced Forest, 
#Other Shrub and Grass, 
#Leucaena Leucocephala (Tangantangan), 
#Native Limestone Forest

#ok.... let's try a different approach --> reclassify veg_sf & then make vegext
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

table(veg_sf$CLASS) #old version 

veg_sf$CLASS <- recode_CLASS(veg_sf$CLASS)

table(veg_sf$CLASS) #new version -- looks good!!

#remaking vegext
veg_sp <- as(veg_sf, "Spatial")
table(veg_sp$CLASS) #good

vegext <- make.mask(
  ch.traps,
  buffer = 2000, #MAY NEED TO CHANGE THIS 
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

#covariates in vegext
#session 1
table(covariates(vegext)[[1]]$CLASS)
#session 2
table(covariates(vegext)[[2]]$CLASS) #good

#save veg ext
saveRDS(vegext, file = "veg_mask_reclass1.rds")
vegext <- readRDS("veg_mask_reclass1.rds")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#TO DO 4/21/26:
# 1. check what poly = does
  #GOOD - instead of creating a circular buffer around traps, the mask is clipped to your habitat shapefile geometry.
  #buffer defines potential movement space
  #poly defines habitat-constrained state space

# 2. reclassify down to ~5 (focus on what is at cams) -- GOOD, may need to change categories

# 3. make NA adjacent class type or mask/remove them -MAY NOT NEED?
# 4. put cats that were only captured once back in! --- GOOD
# 5. plot density vs buffer size (500, 1000, 15000 etc.) -- GOOD
# 6. find distance from edge of island to camera trap array/create plot? -- GOOD
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


#Version 2: NEW Veg extent models~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Null model: Hazard Rate
fit_null_HZ <- secr.fit(
  ch,
  mask = vegext,
  model = list(D ~ 1, g0 ~ 1, sigma ~ 1),
  detectfn = 1
)

fit_null_HZ

saveRDS(fit_null, file = "null_b2000_vegext_s100_HZ_model.rds")
fit_null_HZ <- readRDS("null_b2000_vegext_s100_HZ_model.rds")

# Null model: Halfnormal
fit_null_HN <- secr.fit(
  ch,
  mask = vegext,
  model = list(D ~ 1, g0 ~ 1, sigma ~ 1),
  detectfn = 0
)

saveRDS(fit_null_HN, file = "null_b2000_vegext_s100_HN_model.rds")
fit_null_HN <- readRDS("null_b2000_vegext_s100_HN_model.rds")

#Null model: Exponential
fit_null_EX <- secr.fit(
  ch,
  mask = vegext,
  model = list(D ~ 1, g0 ~ 1, sigma ~ 1),
  detectfn = 2
)

saveRDS(fit_null_EX, file = "null_b2000_vegext_s100_EX_model.rds")
fit_null_EX <- readRDS("null_b2000_vegext_s100_EX_model.rds")

#Compare null models
AIC(fit_null_HZ, fit_null_HN, fit_null_EX) #HZ (detectfn = 1) still the best detectfn 
#......................................................................

# Habitat model: D ~ CLASS 
fit_Dclass <- secr.fit(
  ch,
  mask = vegext,
  model = list(D ~ CLASS, g0 ~ 1, sigma ~ 1),
  detectfn = 1
) #WARNING --- VERY SLOW 

#Warning message:
#In secr.fit(ch, mask = vegext, model = list(D ~ CLASS, g0 ~ 1, sigma ~  :
            #at least one variance calculation failed 

#DID NOT WORK PROPERLY ---> MISSING CI ESTIMATES
# WILL STILL SAVE BUT DO NOT USE

saveRDS(fit_Dclass, file = "DxCLASS_b2000_vegext_s100_HZ_model.rds")
fit_DclassHN <- readRDS("DxCLASS_b2000_vegext_s100_HZ_model.rds")

#fit_DclassHN did work & results saved in object (DxCLASS_b2000_vegext_s100_HN_model.rds)
AIC(fit_DclassHN, fit_null_HN, fit_null_HZ)

#MUST'VE NOT LIKED THE EXTRA PARAMETER (z) in HZ

#compare models
AIC(fit_null_HZ, fit_Dclass) #null has lower AIC by 1.543

predict(fit_Dclass) #just did not work.......

predict(fit_Dclass)

coef(fit_Dclass)
summary(fit_Dclass)

#WILL NEED TO PAIR DOWN EVEN MORE & SEE IF THAT WORKS!!!!!!!!!!

#Final Analysis............######################################################################################
# Compare null models.........................................
fit_null_HZ <- readRDS("null_b2000_vegext_s100_HZ_model.rds")
fit_null_EX <- readRDS("null_b2000_vegext_s100_EX_model.rds")
fit_null_HN <- readRDS("null_b2000_vegext_s100_HN_model.rds")
AIC(fit_null_HZ, fit_null_HN, fit_null_EX) #HZ (detectfn = 1) still the best detectfn 

# Session model: D ~ session ..................................
fit_Dsess <- secr.fit(
  ch,
  mask = vegext,
  model = list(D ~ session, g0 ~ 1, sigma ~ 1),
  detectfn = 1
)

saveRDS(fit_Dsess, file = "Dxsess_b2000_vegext_s100_HZ_model.rds")
fit_Dsess <- readRDS("Dxsess_b2000_vegext_s100_HZ_model.rds")

# Session model: g0 ~ session ..................................
fit_g0sess <- secr.fit(
  ch,
  mask = vegext,
  model = list(D ~ 1, g0 ~ session, sigma ~ 1),
  detectfn = 1
)

saveRDS(fit_g0sess, file = "g0sess_b2000_vegext_s100_HZ_model.rds")
fit_g0sess <- readRDS("g0sess_b2000_vegext_s100_HZ_model.rds")

# Session model: sigma ~ session ..............................
fit_sigmasess <- secr.fit(
  ch,
  mask = vegext,
  model = list(D ~ 1, g0 ~ 1, sigma ~ session),
  detectfn = 1
)

saveRDS(fit_sigmasess, file = "sigmasess_b2000_vegext_s100_HZ_model.rds")
fit_sigmasess <- readRDS("sigmasess_b2000_vegext_s100_HZ_model.rds")

# Session model: D ~ session, g0 ~ session, sigma ~ session ............
fit_full_sess <- secr.fit(
  ch,
  mask = vegext,
  model = list(D ~ session, g0 ~ session, sigma ~ session),
  detectfn = 1
)

saveRDS(fit_full_sess, file = "fullsess_b2000_vegext_s100_HZ_model.rds")
fit_full_sess <- readRDS("fullsess_b2000_vegext_s100_HZ_model.rds")

#compare models .................................................
AIC(fit_null_HZ, fit_Dsess, fit_g0sess, fit_sigmasess, fit_full_sess)
#D ~ session has the lowest AIC by 1.299 compared to null HZ model

#Reclassify vegext: Try 2 ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
veg_sf <- st_read("C:/Users/celin/Tinian-Cat-occupancy-/scr-analysis/CNMI Hi-Res veg data/tinian_release.shp") %>%
  st_transform(crs = st_crs(traps.sf))

#look at CLASS at cam trap locations
table(traps$CLASS.landcover) 

#includes: Casuarina Thicket 2, 
#Mixed Introduced Forest 17, 
#Other Shrub and Grass 7, 
#Leucaena Leucocephala (Tangantangan) 23, 
#Native Limestone Forest 1

#look at CLASS in previous vegext --> 6 covariates --> TOO MANY
#session 1
table(covariates(vegext)[[1]]$CLASS)
#session 2
table(covariates(vegext)[[2]]$CLASS)


#ok.... let's try a different approach --> reclassify try 2
recode_CLASS <- function(x) {
  
  x <- trimws(as.character(x))
  
  x[is.na(x) | x == "" | x == "NA"] <- "NoVegData"
  
  x[x %in% c("Native Limestone Forest", 
             "Mixed Introduced Forest")] <- "NativeMixedForest"
  
  x[x %in% c("Leucaena Leucocephala (Tangantangan)", 
             "Agroforest", 
             "Agroforest -- Coconut",
             "Casuarina Thicket")] <- "IntroducedForest"
  
  x[x %in% c("Other Shrub and Grass",
             "Cropland", 
             "Urban and Built-up",
             "Urban Vegetation",
             "Strand",
             "Barren/Sandy Beach/Bare Rocks",
             "Wetland")] <- "UnForested"
  
  x
}

#IS IRONWOOD NATIVE OR INTRODUCED????

table(veg_sf$CLASS) #old version 

veg_sf$CLASS <- recode_CLASS(veg_sf$CLASS)

table(veg_sf$CLASS) #new version: 3 CLASSES -- looks good!!

#remaking vegext
veg_sp <- as(veg_sf, "Spatial")
table(veg_sp$CLASS) #good

vegext2 <- make.mask(
  ch.traps,
  buffer = 2000, 
  spacing = 100,
  type = "polygon", #not listed as an option.....
  poly = veg_sp
)

vegext2 <- addCovariates(
  object = vegext2,
  spatialdata = veg_sf,
  columns = "CLASS"
)

vegext2 <- shareFactorLevels(vegext2)
verify(vegext2) #nice
plot(vegext2)

str(vegext2)

#checking covariates in vegext2
#session 1
table(covariates(vegext2)[[1]]$CLASS)
#session 2
table(covariates(vegext2)[[2]]$CLASS) #good -- looks evenly spread across CLASS :)

# save veg ext
saveRDS(vegext2, file = "veg_mask_reclass2.rds")
vegext2 <- readRDS("veg_mask_reclass2.rds")

#Try habitat model again w/ HZ detectfn
# Habitat model: D ~ CLASS  ................................................
# Going to keep D ~ session out for now

fit_Dclass2 <- secr.fit(
  ch,
  mask = vegext2,
  model = list(D ~ CLASS, g0 ~ 1, sigma ~ 1),
  detectfn = 1 #HZ
) #WARNING --- VERY SLOW 

saveRDS(fit_Dclass2, file = "DxCLASS2_b2000_vegext_s100_HZ_model.rds")
fit_Dclass2 <- readRDS("DxCLASS2_b2000_vegext_s100_HZ_model.rds")

#compare models ............................
AIC(fit_Dclass2, fit_Dsess, fit_null_HZ) #Dsess, Dclass2, then null all within 3 AIC

summary(fit_null_HZ)
predict(fit_Dclass2) 

coef(fit_Dclass2)
summary(fit_Dclass2)

# Habitat & Session model: D ~ CLASS + session
fit_Dclass_sess <- secr.fit(
  ch,
  mask = vegext2,
  model = list(D ~ CLASS + session, g0 ~ 1, sigma ~ 1),
  detectfn = 1 #HZ
) #WARNING --- VERY SLOW 

#Warning messages:
#  1: In secr.fit(ch, mask = vegext2, model = list(D ~ CLASS + session,  :
 #                                                   possible maximization error: nlm returned code 4. See ?nlm
  #                                                2: In secr.fit(ch, mask = vegext2, model = list(D ~ CLASS + session,  :
     #                                                                                               at least one variance calculation failed 

saveRDS(fit_Dclass_sess, file = "DxCLASSxsess_b2000_vegext_s100_HZ_model.rds")
fit_Dclass_sess <- readRDS("DxCLASSxsess_b2000_vegext_s100_HZ_model.rds")

predict(fit_Dclass_sess)
summary(fit_Dclass_sess) # failed to calculate NativeMixedForest
#compare models ............................
AIC(fit_Dclass2, fit_Dsess, fit_null_HZ, fit_Dclass_sess) 

#Habitat detection model: g0 ~ CLASS
#fit_g0class <- secr.fit(
 # ch,
  #mask = vegext2,
  #model = list(D ~ 1, g0 ~ CLASS, sigma ~ 1),
  #detectfn = 1 #HZ
#) ---> NOPE NEED THE COVARIATE TO BE SITE-LEVEL

# Behavioral model: g0 ~ b ......................
fit_g0b <- secr.fit(
  ch,
  mask = vegext2,
  model = list(D ~ 1, g0 ~ b, sigma ~ 1),
  detectfn = 1 #HZ
) 

saveRDS(fit_g0b, file = "g0b_b2000_vegext_s100_HZ_model.rds")
fit_g0b <- readRDS("g0b_b2000_vegext_s100_HZ_model.rds")

AIC(fit_g0b, fit_null_HZ, fit_g0sess)  #models not compatible for AIC...

predict(fit_g0b) 

coef(fit_g0b)
summary(fit_g0b)

#time as a factor .......................................
fit_g0t <- secr.fit(
  ch,
  mask = vegext2,
  model = list(D ~ 1, g0 ~ t, sigma ~ 1),
  detectfn = 1 #HZ
)  # took 10 hours

saveRDS(fit_g0t, file = "g0t_b2000_vegext_s100_HZ_model.rds")
fit_g0t <- readRDS("g0t_b2000_vegext_s100_HZ_model.rds")

AIC(fit_g0b, fit_g0t)

#time as a trend ..........................................
fit_g0T <- secr.fit(
  ch,
  mask = vegext2,
  model = list(D ~ 1, g0 ~ T, sigma ~ 1),
  detectfn = 1 #HZ
) 

saveRDS(fit_g0T, file = "g0T_b2000_vegext_s100_HZ_model.rds")
fit_g0T <- readRDS("g0T_b2000_vegext_s100_HZ_model.rds")

AIC(fit_g0b, fit_g0t, fit_g0T) #g0~b best fitting by >3 delta AIC

#Completed in 48339.06 seconds at 11:33:52 05 May 2026 
#Warning message:
 # In secr.fit(ch, mask = vegext2, model = list(D ~ 1, g0 ~ T, sigma ~  :
  #                                               at least one variance calculation failed 


#Checking for distance from edge of island to camera array ###############################################################
island_boundary <- st_boundary(st_union(veg_sf))
plot(island_boundary)

traps.sf <- st_as_sf(traps, coords = c("x", "y"), crs = 32655)

dist_matrix <- st_distance(traps.sf, island_boundary) #in meters

min_dist_traps <- apply(dist_matrix, 1, min)

min(min_dist_traps)      # 80 m closest trap to edge
max(min_dist_traps)      # 3107 m furthest trap from edge
mean(min_dist_traps)     # 1310 m average distance

plot(island_boundary, col = "black")
plot(traps.sf, add = TRUE)

hist(min_dist_traps,
     main = "Distance from traps to island edge",
     xlab = "Distance (meters)")

#distance from array centroid to edge
array_centroid <- st_centroid(st_union(traps.sf))
st_distance(array_centroid, island_boundary) #1967 m 

#ploting buffer size and island edge
array_center <- st_centroid(st_union(traps.sf))

buffers <- c(500, 1000, 1500, 2000, 2500, 3000)

buffer_list <- lapply(buffers, function(b) {
  st_buffer(array_center, dist = b)
})

buffer_sf <- do.call(rbind, lapply(seq_along(buffer_list), function(i) {
  st_sf(distance = buffers[i], geometry = buffer_list[[i]])
}))


ggplot() +
  geom_sf(data = island_boundary, color = "black") +
  geom_sf(data = buffer_sf, aes(color = as.factor(distance)), fill = NA) +
  geom_sf(data = traps.sf, color = "grey", size = 2) +
  geom_sf(data = array_center, color = "red", size = 3) +
  labs(color = "Buffer (m)",
       title = "Camera Trap Array Buffers vs Island Edge") +
  theme_minimal()

#plotting trap buffers
buffers <- c(500, 1000, 2000, 3000)

trap_buffers <- lapply(buffers, function(b) {
  st_buffer(traps.sf, dist = b) %>%
    mutate(buffer_m = b)
})
trap_buffers_sf <- do.call(rbind, trap_buffers)

ggplot() +
  geom_sf(data = island_boundary, color = "black") +
  geom_sf(data = trap_buffers_sf,
          aes(color = as.factor(buffer_m)),
          fill = NA,
          alpha = 0.4) +
  geom_sf(data = traps.sf, color = "black", size = 1.5) +
  labs(color = "Buffer (m)",
       title = "Buffers Around Each Camera Trap") +
  theme_minimal()

#plotting w/ buffers clipped to island boundary
buffers_plot <- c(500, 1500, 3000)
trap_buffers_plot <- lapply(buffers_plot, function(b) {
  st_buffer(traps.sf, dist = b) %>%
    mutate(buffer_m = b)
})

trap_buffers_plot_sf <- do.call(rbind, trap_buffers_plot)
trap_buffers_clipped <- st_intersection(trap_buffers_plot_sf, veg_sf)

ggplot() +
  geom_sf(data = island_boundary, color = "black") +
  geom_sf(data = trap_buffers_clipped,
          aes(color = as.factor(buffer_m)),
          fill = NA,
          linewidth = 0.3) +
  geom_sf(data = traps.sf, color = "black", size = 2) +
  labs(color = "Buffer (m)",
       title = "Trap Buffers Clipped to Island",
       subtitle = "Explains why density stabilizes across buffer sizes") +
  theme_minimal()

trap_buffers_plot <- lapply(buffers_plot, function(b) {
  st_buffer(traps.sf, dist = b) %>%
    mutate(buffer_m = b)
})

trap_buffers_plot_sf <- do.call(rbind, trap_buffers_plot)

trap_buffers_clipped <- st_intersection(trap_buffers_plot_sf, veg_sf)

ggplot() +
  geom_sf(data = island_boundary, color = "black") +
  geom_sf(data = trap_buffers_clipped,
          fill = NA,
          color = "blue",
          linewidth = 0.3) +
  geom_sf(data = traps.sf, color = "black", size = 2) +
  facet_wrap(~buffer_m) +
  labs(title = "Trap Buffers Clipped to Island",
       subtitle = "Each panel shows a different buffer size") +
  theme_minimal()