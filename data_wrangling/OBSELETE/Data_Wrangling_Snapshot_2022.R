# Data Wrangling Snapshot 2022
# Margaret Mercer
# January 12, 2024

library(dplyr)
library(suncalc)

obs_2022 <- read.csv("data/updated_2022_sequences.csv")
dep_2022 <- read.csv("data/updated_2022_deployments.csv")

left_joined_22 <- left_join(obs_2022, dep_2022, by = "deployment_id")
all_2022 <- left_joined_22[, c("subproject_name", "deployment_id", "survey_days",  "latitude", "longitude", "start_time", "genus", "species", "common_name", "group_size")] #"subproject_name" is comparable to "array" in 2019 and 2020 data

# put genus and species together to make a "species_name" column
all_2022$Species_Name <- paste(all_2022$genus, all_2022$species, sep = " ")
# if Species_Name is empty, replace it with common_name
all_2022$Species_Name <- ifelse(all_2022$Species_Name == "   ", all_2022$common_name, all_2022$Species_Name)
all_2022 <- dplyr::select(all_2022, -genus, -species)

# fix date and time formatting
all_2022$start_time <- as.POSIXct(strptime(all_2022$start_time, format = "%Y/%m/%d %H:%M:%S"))

colnames(all_2022)[6] <- "Local_Date_Time"

# look up time zones and associate them with each camera
all_2022$Time_Zone <- tz_lookup_coords(all_2022$latitude, all_2022$longitude, method = "accurate", warn = TRUE)

# fix problem children
all_2022 <- separate(all_2022, Local_Date_Time, c("Date", "Time"), sep = " ") # first we separate dates and times
all_2022$Time[is.na(all_2022$Time)] <- "00:00:01" # now replace all NAs with "00:00:01"
sum(is.na(all_2022$Time)) # check for NAs

all_2022 <- all_2022 %>%
  mutate(Time = if_else(Time == "14:00:00", "14:00:01", Time),
         Time = if_else(Time == "15:00:00", "15:00:01", Time),
         Time = if_else(Time == "16:00:00", "16:00:01", Time),
         Time = if_else(Time == "17:00:00", "17:00:01", Time),
         Time = if_else(Time == "18:00:00", "18:00:01", Time),
         Time = if_else(Time == "19:00:00", "19:00:01", Time),
         Time = if_else(Time == "20:00:00", "20:00:01", Time))

all_2022 <- unite(all_2022, Local_Date_Time, c("Date", "Time"), sep = " ")
all_2022$Local_Date_Time <- as.POSIXct(all_2022$Local_Date_Time)

# NEXT: run from here down to see if it solved the problem!!

# for loop to convert times to UTC. This took about 25 minutes
for(i in 1:nrow(all_2022)) {
  x <- ymd_hms(all_2022$Local_Date_Time[i], tz = all_2022$Time_Zone[i])
  x <- with_tz(x, tzone = "UTC")
  all_2022$time_utc[i] <- as.character(x)
}
all_2022$time_utc <- ymd_hms(all_2022$time_utc)

sum(is.na(all_2022$time_utc))

colnames(all_2022)[4] <- "lat"
colnames(all_2022)[5] <- "lon"
colnames(all_2022)[11] <- "date"

sun_position <- getSunlightPosition(
  data = all_2022,
  keep = c("altitude")
)

all_2022['Altitude'] = sun_position$altitude

all_2022 <- all_2022[, c("subproject_name", "deployment_id", "survey_days", "lat", "lon", "Local_Date_Time", "Species_Name", "common_name", "group_size", "Time_Zone", "Altitude", "date")]

colnames(all_2022)[1] <- "Array"
colnames(all_2022)[2] <- "Site_Name"
colnames(all_2022)[3] <- "Survey_Days"
colnames(all_2022)[4] <- "Latitude"
colnames(all_2022)[5] <- "Longitude"
colnames(all_2022)[6] <- "Local_Date_Time"
colnames(all_2022)[7] <- "Species_Name"
colnames(all_2022)[8] <- "Common_Name"
colnames(all_2022)[9] <- "Count"
colnames(all_2022)[12] <- "UTC_Date_Time"

all_2022$Is_Night <- ifelse(all_2022$Altitude < 0, 1, 0)

library(terra)
library(sf)
library(raster)

full_raster <- rast("/Users/mmercer3/Downloads/ml-hfi_v1_2019.tif.crdownload")
max(full_raster)

# crop dataset from the whole world to just the US

e <- extent(-167, -66, 20, 72)
raster <- terra::crop(full_raster, e)
plot(raster) # cut off at 70* N but that's ok because none of our observations are futher north than that

# associate lats and longs with dataframe

sf_points <- st_as_sf(all_2022, coords = c("Longitude", "Latitude"), crs = 4326)
values_at_points <- terra::extract(raster, sf_points)
rastered <- cbind(all_2022, Raster_Values = values_at_points)
all_2022 <- dplyr::select(rastered, -Raster_Values.ID)
colnames(all_2022)[14] <- "Disturbance"

# Cool. all_2022 now has the deployment, observation, day/night binary, and human disturbance data for 2022!
# You can do this for any year of SnapShotUSA data; simply replace the year number in each of the objects and check lat/long to make sure it still all falls within the range

write.csv(all_2022, "2022.csv", row.names=FALSE)
