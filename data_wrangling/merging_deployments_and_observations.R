# merging deployments and observations in 2019-2023 data
# margaret mercer
# july 25, 2024

library(tidyverse)
library(sp)
library(sf)
library(terra)
library(lubridate)
library(forcats)
library(suncalc)
library(lutz)
library(parallel)
library(foreach)
library(raster)

# import and merge observation and deployment data ####

obs <- read.csv("../data_too_big/SNAPSHOT_USA_Sequences.csv")
dep <- read.csv("../data_too_big/SNAPSHOT_USA_Deployments.csv")

# add year onto deployment id to make each one unique
dep$Deployment_ID <- paste(dep$Deployment_ID, dep$Year, sep = "_")
obs$Deployment_ID <- paste(obs$Deployment_ID, obs$Year, sep = "_")

# check for duplicates
duplicates <- dep %>%
  group_by(Deployment_ID) %>%
  filter(n() > 1)
length(duplicates$Site_Name) # should be 0

# check to be sure both dep and obs have same number of sites
length(unique(dep$Deployment_ID)) # 9707
length(unique(obs$Deployment_ID)) # 9717
# could be because some of the deployments didn't have any observations?
# check which rows are in which
sites_1 <- table(unique(dep$Deployment_ID))
length(sites_1)

sites_2 <- table(unique(obs$Deployment_ID))
length(sites_2)

df_1 <- as.data.frame(sites_1)
df_2 <- as.data.frame(sites_2)

anti_join(df_1, df_2, by = "Var1") # what sites are in deployments but not observations (17) This makes sense; some cameras didn't get deployments
anti_join(df_2, df_1, by = "Var1") # what sites are in observations but not deployments (27) But why this?

# maybe I just cut the ones that are in "observations" but not "deployments", 
    # because there's only 27 and we can't get site-level information like location

joined <- left_join(obs, dep, by = "Deployment_ID") # maybe try left join and see if it does something different
joined$Array <- joined$Camera_Trap_Array.x
joined$Site_Name <- joined$Deployment_ID
joined$Local_Date_Time <- joined$Start_Time
joined$Species_Name <- paste(joined$Genus, joined$Species, sep = " ")
joined <- joined[, c("Array", 
                     "Site_Name", 
                     "Survey_Nights", 
                     "Latitude", 
                     "Longitude", 
                     "Local_Date_Time", 
                     "Species_Name",
                     "Common_Name", 
                     "Habitat",
                     "Development_Level")]

# Ok now we've included only the columns we want.

# suncalc to get altitude and code day/night ####
# look up time zones and associate them with each camera
joined$Time_Zone <- 
  tz_lookup_coords(joined$Latitude, joined$Longitude, method = "accurate", warn = TRUE)

joined <- joined %>%
  filter(!is.na(Time_Zone))

data <- joined

# fix date format
data$Local_Date_Time <- as.POSIXct(strptime(data$Local_Date_Time, format = "%Y/%m/%d %H:%M:%S"))

# fix problem children
data <- separate(data, Local_Date_Time, c("Date", "Time"), sep = " ") # first we separate dates and times
data$Time[is.na(data$Time)] <- "00:00:01" # now replace all NAs with "00:00:01"
sum(is.na(data$Time)) # check for NAs

data <- data %>%
  mutate(Time = if_else(Time == "14:00:00", "14:00:01", Time),
         Time = if_else(Time == "15:00:00", "15:00:01", Time),
         Time = if_else(Time == "16:00:00", "16:00:01", Time),
         Time = if_else(Time == "17:00:00", "17:00:01", Time),
         Time = if_else(Time == "18:00:00", "18:00:01", Time),
         Time = if_else(Time == "19:00:00", "19:00:01", Time),
         Time = if_else(Time == "20:00:00", "20:00:01", Time))

data <- unite(data, Local_Date_Time, c("Date", "Time"), sep = " ") # unite to see if it fixed it?
data$Local_Date_Time <- as.POSIXct(data$Local_Date_Time)

# for loop!
# This takes a long time. Like, an hour probably

for(i in 1:nrow(data)) {
  cat("Starting row ", i,"\n")
  x <- ymd_hms(data$Local_Date_Time[i], tz = data$Time_Zone[i])
  x <- with_tz(x, tzone = "UTC")
  data$time_utc[i] <- as.character(x)

}


data$time_utc <- ymd_hms(data$time_utc)

sum(is.na(data$time_utc))

data_utc <- data

# rename columns so suncalc can recognize them ####
# FIX this so its reproducable
colnames(all_2019)[4] <- "lat"
colnames(all_2019)[5] <- "lon"
colnames(all_2019)[11] <- "date"

data <- data %>%
  rename(
    lat = Latitude,
    lon = Longitude,
    date = time_utc
  )

# get altitudes
sun_position <- getSunlightPosition(
  data = data,
  keep = c("altitude")
)

data['Altitude'] = sun_position$altitude

# let's rename some columns so they look nice
data <- data %>%
  rename(
    Latitude = lat,
    Longitude = lon,
    UTC_Date_Time = date
  )



# now let's code 1s and 0s for altitude (sun position) (1 for night observation, 0 for day observation)
# so we want a 1 if it's negative, and a 0 if it's positive
data$Is_Night <- ifelse(data$Altitude < 0, 1, 0)


# clean up column names, standardize common names, etc ####

# make species_name "vehicle" for vehicles
data <- data %>%
  mutate(Species_Name = if_else(Common_Name == "Vehicle", "Vehicle", Species_Name))
# trim all white space
data$Species_Name <- trimws(data$Species_Name)
# add unique number for each row
data$record_ID <- c(1:length(data$Array))
# change Survey_Nights "0"s to "1" (it was out a part of a day, not really ZERO days)
data$Survey_Nights <- replace(data$Survey_Nights, data$Survey_Nights == 0, 1)

data$Human <- ifelse(data$Species_Name == "Homo sapiens" | data$Species_Name == "Vehicle", 1, 0)
Humans_Per_Camera <- data %>% filter(Human == 1) %>%
  group_by(Site_Name) %>%
  summarise(Humans_Per_Camera = n())
data <- left_join(data, Humans_Per_Camera, by = "Site_Name") %>%
  mutate(Humans_Per_Camera = tidyr::replace_na(Humans_Per_Camera, 0)) # replace nas with 0
data$Humans_Per_Camera_Per_Day <- data$Humans_Per_Camera/data$Survey_Nights
data <- subset(data, select = c(-Human))


# add column for human disturbance index
full_raster <- rast("/Users/mmercer3/Downloads/ml-hfi_v1_2019.tif.crdownload")
max(full_raster)
# crop dataset from the whole world to just the US:
e <- extent(-167, -66, 20, 72)
raster <- terra::crop(full_raster, e)
plot(raster) # to be sure it actually includes US like we want it to. 
# Looks like Alaska is slightly cut off (at 70*) but our highest lat is only 59* so that's ok
# associate lats and longs with dataframe:
sf_points <- st_as_sf(data, coords = c("Longitude", "Latitude"), crs = 4326)
values_at_points <- terra::extract(raster, sf_points)
rastered <- cbind(data, Raster_Values = values_at_points)
data <- dplyr::select(rastered, -Raster_Values.ID)
data <- rename(data, Disturbance = `Raster_Values.ml-hfi_v1_2019.tif`)

# write csv
write.csv(data, "../data_too_big/five_year_observation_data.csv")
