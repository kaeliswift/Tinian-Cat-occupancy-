#SECR Workshop Incorportation 
#4/6/26

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

#checking for only 1 encounter......
table(data.clean$Individuals)
length(unique(data.clean$Individuals)) #47 individuals

#REMOVING INDIVIDUALS W/ ONLY 1 ENCOUNTER
counts <- table(data.clean$Individuals)
data.filtered <- data.clean[data.clean$Individuals %in% names(counts[counts > 1]), ]
length(unique(data.filtered$Individuals)) #26 individuals 

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

#Incorporation of Workshop Code#########################################################

#ways to check out data stuffs
summary(ch)

#run the null model with a half normal detection
cats.HN<-secr.fit(ch, buffer=3000)

cats.HN

#get D, g0, and sigma on the real line (back-transformed from the link function)
predict(cats.HN)

#calculate the confidence interval for a parameter
confint(cats.HN, "D") #possible error???

#run the null model with a half normal detection
cats.test<-secr.fit(ch, buffer=100) #I feel like this is the similar as line 225 (cats.HN)

#what buffer should I use?
detpar <-  list(g0 = 0.27, sigma = 217)  #info from stoatHN1
str(detpar)
suggest.buffer(ch.traps, "halfnormal", detpar, 6)  #6 occasions
#ISSUE --> could it be sessions???
ch.ses1 <- subset(ch, sessions = 1)
suggest.buffer(traps(ch.ses1), "halfnormal", detpar, 6)  #6 occasions
#720 m seems small???
ch.ses2 <- subset(ch, sessions = 2)
suggest.buffer(traps(ch.ses2), "halfnormal", detpar, 6)  #6 occasions
#757 also small???

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

suggest.buffer(cats.HZ)


#changing buffer to 2000
HN.2000 <-secr.fit(ch, buffer=2000, detectfn = 0)
HN.2000
HZ.2000 <- secr.fit(ch, buffer = 2000, detectfn = 1)
HZ.2000
EX.2000 <- secr.fit(ch, buffer = 2000, detectfn = 2)
EX.2000

#compare AICs
AIC(HN.2000, HZ.2000, Ex.2000) #buffer = 2000, worse AICs HZ is still the best model
AIC(cats.HN, cats.HZ, cats.EX) #buffer = 3000
#are these comparable?

#Playing around with different buffer sizes ########################################

#trying different buffer fits...................................
buffers <- c(2000, 3000, 4000, 5000)

fits <- lapply(buffers, function(b) {
  secr.fit(capthist = ch, buffer = b, detectfn = 1)
})


#session 1
sapply(fits, function(fit) {
  derived(fit)[[1]]["D","estimate"]})
#session 2 
sapply(fits, function(fit) {
  derived(fit)[[2]]["D","estimate"]})

#both sessions 
sapply(fits, function(fit) {
  sapply(derived(fit), function(x) x["D","estimate"])
}) 

#density estimates still decreasing -> increase buffer size..........

buffers <- c(3000, 5000, 7000, 10000)

fits <- lapply(buffers, function(b) {
  secr.fit(capthist = ch, buffer = b, detectfn = 1)
})


#session 1
sapply(fits, function(fit) {
  derived(fit)[[1]]["D","estimate"]})
#session 2 
sapply(fits, function(fit) {
  derived(fit)[[2]]["D","estimate"]})

#both sessions 
sapply(fits, function(fit) {
  sapply(derived(fit), function(x) x["D","estimate"])
}) 


#trying with HN instead to see if density stabilizes...........
buffers <- c(3000, 5000, 7000, 10000)

fits <- lapply(buffers, function(b) {
  secr.fit(capthist = ch, buffer = b, detectfn = 0)
})

#session 1
sapply(fits, function(fit) {
  derived(fit)[[1]]["D","estimate"]})
#session 2 
sapply(fits, function(fit) {
  derived(fit)[[2]]["D","estimate"]})

#both sessions 
sapply(fits, function(fit) {
  sapply(derived(fit), function(x) x["D","estimate"])
}) 



#buffer sensitivity check.......................................................
#buffers <- c(2000, 3000, 4000, 5000, 6000, 7000) #checking 2 to 7 km 

#fits <- lapply(buffers, function(b) {
 # secr.fit(ch, buffer = b)}) #fitting all the buffers

#checking if D estimate stabilizes

#session 1
#sapply(fits, function(fit) {
 # derived(fit)[[1]]["D","estimate"]})
#session 2 
#sapply(fits, function(fit) {
 # derived(fit)[[2]]["D","estimate"]})

#both sessions 
#sapply(fits, function(fit) {
 # sapply(derived(fit), function(x) x["D","estimate"])
#})

#graphing
#D_values <- sapply(fits, function(fit) {
  #sapply(derived(fit), function(x) x["D","estimate"])
#})

#matplot(buffers, t(D_values), type = "b", pch = 1:2, col = 1:2,
 #       xlab = "Buffer (m)", ylab = "D estimate", lty = 1:2)
#legend("topright", legend=c("Session 1","Session 2"), pch=1:2, lty=1:2)


#stabilized buffer
# Calculate relative change between successive D estimates
#rel_change <- abs(diff(D_estimates) / D_estimates[-length(D_estimates)])

# Show which buffers are within 5% change
#stabilized_idx <- which(rel_change < 0.10) 
#buffers[stabilized_idx + 1]  # +1 because diff shifts index
#optimal_buffer <- buffers[min(stabilized_idx + 1)]
#optimal_buffer

#Buffer test for cats.HZ
#buffers <- seq(500, 3500, by = 250)

#buffer_results <- data.frame(
 # buffer = buffers,
 # AIC = NA_real_,
  #logLik = NA_real_
#)

#for (i in seq_along(buffers)) {
 # fit <- update(cats.HZ, buffer = buffers[i])
  
  # store AIC and log-likelihood
#  buffer_results$AIC[i] <- AIC(fit)
 # buffer_results$logLik[i] <- logLik(fit)
#}

#buffer_results #going with 3000 m 


#Incorporating a habitat mask ####################################################

#reading in CNMI high-res veg data
tinian <- st_read("C:/Users/celin/Tinian-Cat-occupancy-/scr-analysis/CNMI Hi-Res veg data/tinian_release.shp")

plot(tinian)

#converting to a polygon
library(sp)
tinian_sp <- as(tinian, "Spatial")

#are traps formatted properly?
ch.traps #yes

#do I need to incorporate area boundaries???

#make habitat mask with Tinian high res data
habitat <- make.mask(
  ch.traps,
  buffer = 3000,              # adjust buffer (meters) ---> NEEDS TO BE WHAT YOU WANT IN MODEL
  spacing = 100,              # grid resolution ---> CAN CHANGE THIS // RN GIVES 10 PTS BTW CAMS
  type = "polygon",
  poly = tinian_sp
)

#trying null models w/ habitat mask..............................................
fit0.HN <- secr.fit(
  ch,
  mask = habitat,
  detectfn = "HN",
  model = list(D ~ 1, g0 ~ 1, sigma ~ 1)
)

fit0.HN

hold=predictDsurface(fit0.HN, mask = habitat, alpha = 0.05)
plot(hold) #looks weird --> may need to constrain to MLA?????

covariates(habitat) #class never transferred data w/ polygon


#WORK ON  CONVERTING MASK COVARIATE DATA...........
class(habitat)
covariates(habitat) #class never transferred data w/ polygon
names(covariates(habitat))
str(covariates(habitat))

#convert mask to sf points
mask_sf <- st_as_sf(
  as.data.frame(habitat),
  coords = c("x", "y"),
  crs = st_crs(tinian)
) #didn't work most likely sessions

str(habitat[[1]]) #habitat is included but hidden w/in


#extracting mask points
mask_points <- data.frame(
  x = habitat[[1]]$x,
  y = habitat[[1]]$y
)

veg_sf <- st_read("C:/Users/celin/Tinian-Cat-occupancy-/scr-analysis/CNMI Hi-Res veg data/tinian_release.shp")

#spatial join
mask_sf <- st_as_sf(mask_points, coords = c("x","y"), crs = st_crs(veg_sf))

#DIDNT WORK..... CONTINUE ON THIS (think its CLASS name):
join <- st_join(mask_sf, veg_sf["CLASS.landcover"])
