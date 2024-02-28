# Data Wrangling Snapshot 2019
# (partner document is "Test_Data_Wrangling.R")
# Margaret Mercer
# December 11, 2023

obs_2019 <- read.csv("SNAPSHOT_USA_2019_observations.csv")
dep_2019 <- read.csv("SNAPSHOT_USA_2019_deployments.csv")

## merge deployment and observation data
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

left_joined_19 <- left_join(obs_2019, dep_2019, by = "Site_Name") # maybe try left join and see if it does something different
all_2019 <- left_joined_19[, c("Camera_Trap_Array.x", "Site_Name", "Survey_Days", "Latitude.x", "Longitude.x", "Begin_Time", "Species_Name", "Common_Name", "Count")]
# Ok now we've included only the columns we want.

colnames(all_2019)[6] <- "Local_Date_Time"

# look up time zones and associate them with each camera
all_2019$Time_Zone <- tz_lookup_coords(all_2019$Latitude.x, all_2019$Longitude.x, method = "accurate", warn = TRUE)

# fix date format
all_2019$Local_Date_Time <- as.POSIXct(strptime(all_2019$Local_Date_Time, format = "%m/%d/%y %H:%M"))

# fix problem children
all_2019 <- separate(all_2019, Local_Date_Time, c("Date", "Time"), sep = " ") # first we separate dates and times
all_2019$Time[is.na(all_2019$Time)] <- "00:00:01" # now replace all NAs with "00:00:01"
sum(is.na(all_2019$Time)) # check for NAs

all_2019 <- all_2019 %>%
  mutate(Time = if_else(Time == "14:00:00", "14:00:01", Time),
         Time = if_else(Time == "15:00:00", "15:00:01", Time),
         Time = if_else(Time == "16:00:00", "16:00:01", Time),
         Time = if_else(Time == "17:00:00", "17:00:01", Time),
         Time = if_else(Time == "18:00:00", "18:00:01", Time),
         Time = if_else(Time == "19:00:00", "19:00:01", Time),
         Time = if_else(Time == "20:00:00", "20:00:01", Time))

all_2019 <- unite(all_2019, Local_Date_Time, c("Date", "Time"), sep = " ") # unite to see if it fixed it?
all_2019$Local_Date_Time <- as.POSIXct(all_2019$Local_Date_Time)

# for loop!
# This took about 10-15 minutes

for(i in 1:nrow(all_2019)) {
  x <- ymd_hms(all_2019$Local_Date_Time[i], tz = all_2019$Time_Zone[i])
  x <- with_tz(x, tzone = "UTC")
  all_2019$time_utc[i] <- as.character(x)
}
all_2019$time_utc <- ymd_hms(all_2019$time_utc)

sum(is.na(all_2019$time_utc))
# second runthrough, since fixing the "00:00"s, those ones worked. Now it's just the 97 that are problems
# nas <- all_2019[is.na(all_2019$time_utc), ]
# nas
# obviously lots of 00:00:00. A lot of 17:00:00, 19:00:00, 20:00:00, and some 14:00:00.
# Ew. WHY?? There are a lot of rows with exact hours (ex row 141), that convert just fine.
# IT WORKED AHHHH
# you just have to change all the ones that end in "00:00" to "00:01" :')

# rename columns so suncalc can recognize them
colnames(all_2019)[4] <- "lat"
colnames(all_2019)[5] <- "lon"
colnames(all_2019)[11] <- "date"

# get altitudes
sun_position <- getSunlightPosition(
  data = all_2019,
  keep = c("altitude")
)

all_2019['Altitude'] = sun_position$altitude

# let's rename some columns so they look nice
colnames(all_2019)[1] <- "Array"
colnames(all_2019)[2] <- "Site_Name"
colnames(all_2019)[4] <- "Latitude"
colnames(all_2019)[5] <- "Longitude"
colnames(all_2019)[6] <- "Local_Date_Time"
colnames(all_2019)[11] <- "UTC_Date_Time"


# now let's code 1s and 0s for altitude (sun position) (1 for night observation, 0 for day observation)
# so we want a 1 if it's negative, and a 0 if it's positive
all_2019$Is_Night <- ifelse(all_2019$Altitude < 0, 1, 0)


# add column for human disturbance index

full_raster <- rast("/Users/mmercer3/Downloads/ml-hfi_v1_2019.tif.crdownload")
max(full_raster)

# crop dataset from the whole world to just the US

e <- extent(-167, -66, 20, 72)
raster <- terra::crop(full_raster, e)
plot(raster) # to be sure it actually includes US like we want it to. Looks like Alaska is slightly cut off (at 70*) but our highest lat is only 59* so that's ok

# associate lats and longs with dataframe

sf_points <- st_as_sf(all_2019, coords = c("Longitude", "Latitude"), crs = 4326)
values_at_points <- terra::extract(raster, sf_points)
rastered <- cbind(all_2019, Raster_Values = values_at_points)
all_2019 <- dplyr::select(rastered, -Raster_Values.ID)
colnames(all_2019)[14] <- "Disturbance"

# here's how to see if times are missing!!
# filter(all_2019, stringr::str_length(Date_Time) < 11)

# all done! all_2019 now has the deployment, observation, day/night binary, and human disturbance data for 2019!

write.csv(all_2019, "2019.csv", row.names=FALSE)

