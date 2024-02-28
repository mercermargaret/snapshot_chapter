# Data Wrangling Snapshot 2020
# (same thing as done in "Data_Wrangling_Snapshot_2019"; that document contains explanatory comments)
# Margaret Mercer
# December 15, 2023

library(dplyr)
library(suncalc)
library(terra)
library(sf)

obs_2020 <- read.csv("SNAPSHOT_USA_2020_observations.csv")
dep_2020 <- read.csv("SNAPSHOT_USA_2020_deployments.csv")

## all over again, with 2020 data
# not all have deployment IDs, so had to merge by site name!

left_joined_20 <- left_join(obs_2020, dep_2020, by = "Site_Name")
all_2020 <- left_joined_20[, c("Camera_Trap_Array.x", "Site_Name", "Survey_Days", "Latitude", "Longitude", "Begin.Time", "Species_Name", "Common_Name", "Count")]

colnames(all_2020)[6] <- "Local_Date_Time"

# look up time zones and associate them with each camera
all_2020$Time_Zone <- tz_lookup_coords(all_2020$Latitude, all_2020$Longitude, method = "accurate", warn = TRUE)

all_2020$Local_Date_Time <- as.POSIXct(all_2020$Local_Date_Time) # convert to POSIXct

# fix problem children
all_2020 <- separate(all_2020, Local_Date_Time, c("Date", "Time"), sep = " ") # first we separate dates and times
all_2020$Time[is.na(all_2020$Time)] <- "00:00:01" # now replace all NAs with "00:00:01"
sum(is.na(all_2020$Time)) # check for NAs

all_2020 <- all_2020 %>%
  mutate(Time = if_else(Time == "14:00:00", "14:00:01", Time),
         Time = if_else(Time == "15:00:00", "15:00:01", Time),
         Time = if_else(Time == "16:00:00", "16:00:01", Time),
         Time = if_else(Time == "17:00:00", "17:00:01", Time),
         Time = if_else(Time == "18:00:00", "18:00:01", Time),
         Time = if_else(Time == "19:00:00", "19:00:01", Time),
         Time = if_else(Time == "20:00:00", "20:00:01", Time))

all_2020 <- unite(all_2020, Local_Date_Time, c("Date", "Time"), sep = " ") # unite to see if it fixed it?
all_2020$Local_Date_Time <- as.POSIXct(all_2020$Local_Date_Time)

# for loop to create a time_utc column. took about 15 minutes
for(i in 1:nrow(all_2020)) {
  x <- ymd_hms(all_2020$Local_Date_Time[i], tz = all_2020$Time_Zone[i])
  x <- with_tz(x, tzone = "UTC")
  all_2020$time_utc[i] <- as.character(x)
}
all_2020$time_utc <- ymd_hms(all_2020$time_utc)

sum(is.na(all_2020$time_utc))

colnames(all_2020)[4] <- "lat"
colnames(all_2020)[5] <- "lon"
colnames(all_2020)[11] <- "date"

# date and time already in correct format!

sun_position <- getSunlightPosition(
  data = all_2020,
  keep = c("altitude")
)

all_2020['Altitude'] = sun_position$altitude

colnames(all_2020)[1] <- "Array"
colnames(all_2020)[2] <- "Site_Name"
colnames(all_2020)[4] <- "Latitude"
colnames(all_2020)[5] <- "Longitude"
colnames(all_2020)[11] <- "UTC_Date_Time"

all_2020$IsNight <- ifelse(all_2020$Altitude < 0, 1, 0)


full_raster <- rast("/Users/mmercer3/Downloads/ml-hfi_v1_2019.tif.crdownload")
max(full_raster)

# crop dataset from the whole world to just the US

e <- extent(-167, -66, 20, 72)
raster <- terra::crop(full_raster, e)
plot(raster) # cut off at 70* N but that's ok because none of our observations are futher north than that

# associate lats and longs with dataframe

sf_points <- st_as_sf(all_2020, coords = c("Longitude", "Latitude"), crs = 4326)
values_at_points <- terra::extract(raster, sf_points)
rastered <- cbind(all_2020, Raster_Values = values_at_points)
all_2020 <- dplyr::select(rastered, -Raster_Values.ID)
colnames(all_2020)[14] <- "Disturbance"

# Cool. all_2020 now has the deployment, observation, day/night binary, and human disturbance data for 2020!
# You can do this for any year of SnapShotUSA data; simply replace the year number in each of the objects and check lat/long to make sure it still all falls within the range

write.csv(all_2020, "2020.csv", row.names=FALSE)
