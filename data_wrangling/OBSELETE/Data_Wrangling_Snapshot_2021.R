# Data Wrangling Snapshot 2021
# Margaret Mercer
# January 12, 2024

library(dplyr)
library(raster)
library(sp)
library(sf)
library(terra)
library(lubridate)
library(forcats)
library(suncalc)
library(lutz)
library(tidyr)

obs_2021 <- read.csv("data/ssusa_2021_sequences.csv")
dep_2021 <- read.csv("data/ssusa_2021_deployments.csv")

left_joined_21 <- left_join(obs_2021, dep_2021, by = "deployment_id")
all_2021 <- left_joined_21[, c("subproject_name", "deployment_id", "start_date", "end_date",  "latitude", "longitude", "start_time", "species_name", "common_name", "group_size")] #"subproject_name" is comparable to "array" in 2019 and 2020 data

# subtract start date from end date to get "Survey_Days" into ONE column, rather than two
Survey_Days <- as.Date(all_2021$end_date) - as.Date(all_2021$start_date)
Survey_Days <- as.numeric(gsub("[^0-9]+", "", Survey_Days))
all_2021$Survey_Days <- Survey_Days
all_2021 <- all_2021[, c("subproject_name", "deployment_id", "Survey_Days",  "latitude", "longitude", "start_time", "species_name", "common_name", "group_size")]

# fix date and time formatting
all_2021$start_time <- as.POSIXct(all_2021$start_time, format = "%m/%d/%Y %H:%M:%S")

colnames(all_2021)[6] <- "Local_Date_Time"

# look up time zones and associate them with each camera
all_2021$Time_Zone <- tz_lookup_coords(all_2021$latitude, all_2021$longitude, method = "accurate", warn = TRUE)

# fix problem children
all_2021 <- separate(all_2021, Local_Date_Time, c("Date", "Time"), sep = " ") # first we separate dates and times
all_2021$Time[is.na(all_2021$Time)] <- "00:00:01" # now replace all NAs with "00:00:01"
sum(is.na(all_2021$Time)) # check for NAs

all_2021 <- all_2021 %>%
  mutate(Time = if_else(Time == "14:00:00", "14:00:01", Time),
         Time = if_else(Time == "15:00:00", "15:00:01", Time),
         Time = if_else(Time == "16:00:00", "16:00:01", Time),
         Time = if_else(Time == "17:00:00", "17:00:01", Time),
         Time = if_else(Time == "18:00:00", "18:00:01", Time),
         Time = if_else(Time == "19:00:00", "19:00:01", Time),
         Time = if_else(Time == "20:00:00", "20:00:01", Time))

all_2021 <- unite(all_2021, Local_Date_Time, c("Date", "Time"), sep = " ") # unite to see if it fixed it?
all_2021$Local_Date_Time <- as.POSIXct(all_2021$Local_Date_Time)

# for loop!
# This took about 15 minutes

for(i in 1:nrow(all_2021)) {
  x <- ymd_hms(all_2021$Local_Date_Time[i], tz = all_2021$Time_Zone[i])
  x <- with_tz(x, tzone = "UTC")
  all_2021$time_utc[i] <- as.character(x)
}
all_2021$time_utc <- ymd_hms(all_2021$time_utc)

sum(is.na(all_2021$time_utc))

colnames(all_2021)[4] <- "lat"
colnames(all_2021)[5] <- "lon"
colnames(all_2021)[11] <- "date"

sun_position <- getSunlightPosition(
  data = all_2021,
  keep = c("altitude")
)

all_2021['Altitude'] = sun_position$altitude

colnames(all_2021)[1] <- "Array"
colnames(all_2021)[2] <- "Site_Name"
colnames(all_2021)[4] <- "Latitude"
colnames(all_2021)[5] <- "Longitude"
colnames(all_2021)[7] <- "Species_Name"
colnames(all_2021)[8] <- "Common_Name"
colnames(all_2021)[9] <- "Count"
colnames(all_2021)[11] <- "UTC_Date_Time"

all_2021$Is_Night <- ifelse(all_2021$Altitude < 0, 1, 0)

full_raster <- rast("/Users/mmercer3/Downloads/ml-hfi_v1_2019.tif.crdownload")

# crop dataset from the whole world to just the US

e <- extent(-167, -66, 20, 72)
raster <- terra::crop(full_raster, e)
plot(raster) # cut off at 70* N but that's ok because none of our observations are futher north than that

# associate lats and longs with dataframe

sf_points <- st_as_sf(all_2021, coords = c("Longitude", "Latitude"), crs = 4326)
values_at_points <- terra::extract(raster, sf_points)
rastered <- cbind(all_2021, Raster_Values = values_at_points)
all_2021 <- dplyr::select(rastered, -Raster_Values.ID)
colnames(all_2021)[14] <- "Disturbance"

# Cool. all_2021 now has the deployment, observation, day/night binary, and human disturbance data for 2021!
# You can do this for any year of SnapShotUSA data; simply replace the year number in each of the objects and check lat/long to make sure it still all falls within the range

write.csv(all_2021, "2021.csv", row.names=FALSE)
