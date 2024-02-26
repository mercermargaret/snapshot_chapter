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

left_joined_19 <- left_join(obs_2019, dep_2019, by = "Site_Name") # maybe try left join and see if it does something different
all_2019 <- left_joined_19[, c("Camera_Trap_Array.x", "Site_Name", "Survey_Days", "Latitude.x", "Longitude.x", "Begin_Time", "Species_Name", "Common_Name", "Count")]
# Ok now we've included only the columns we want.

# rename columns so suncalc can recognize them
colnames(all_2019)[4] <- "lat"
colnames(all_2019)[5] <- "lon"
colnames(all_2019)[6] <- "localtimes"

# all of the below needs to be fixed so that converting times works

# look up time zones and associate them with each camera
all_2019$Time_Zone <- tz_lookup_coords(all_2019$lat, all_2019$lon, method = "accurate", warn = TRUE)

# fix date format
formatted_time <- as.POSIXct(strptime(all_2019$localtimes, format = "%m/%d/%y %H:%M"))
all_2019$localtimes <- formatted_time

## convert times to UTC
# THIS is wrong
# For example it's only pulling Juneau time back 1 hour
all_2019$datetime_utc <- with_tz(all_2019$localtimes, tzone = all_2019$Time_Zone)

# lets try this from ChatGPT
# Convert datetime column to POSIXct object
# all_2019$datetime <- as.POSIXct(all_2019$datetime, tz = all_2019$Time_Zone) 
# PROBLEM: why is this pulling the ORIGINAL time back an hour???

# Convert datetime to UTC
# all_2019$date <- format(all_2019$datetime, tz = "UTC") # Wait...this ISN'T wrong?
# THIS site (https://dateful.com/convert/utc) says: Juneau and Honolulu were correct;
# Boise wasn't :') it was an hour off. So was Denver.
# And New York is THREE hours off
# but now it's saying they're all wrong.....UGH. First line is 13:50. It should be 14:50.
# it's not daylight savings; Jan 31 at 6:50 should be 15:50.

# # from internet:
# # with_tz(all_2019$datetime, tzone = all_2019$Time_Zone)
# x <- ymd_hms(all_2019$datetime, tz = all_2019$Time_Zone)
# with_tz(x, "UTC")
# # this ALSO didn't work :') it had an error message AND did it wrong. Does "tz" argument not take a vector?
#
x <- ymd_hms(all_2019$localtimes, tz = "America/Juneau")
with_tz(x, "UTC")
# this worked...except it didn't like the 661 values :') And you can only do one time zone at a time.
# Is there a way to do this besides just going one by one?


# for loop!
# test <- all  
# it worked!!! 97 failed to parse and it took 15 minutes, but it worked!!:))

for(i in 1:nrow(all_2019)) {
  x <- ymd_hms(all_2019$localtimes[i], tz = all_2019$Time_Zone[i])
  x <- with_tz(x, tzone = "UTC")
  all_2019$time_utc[i] <- as.character(x)
}
all_2019$time_utc <- ymd_hms(all_2019$time_utc)

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
colnames(all_2019)[6] <- "Date_Time"

# now let's code 1s and 0s for altitude (sun position) (1 for night observation, 0 for day observation)
# so we want a 1 if it's negative, and a 0 if it's positive
all_2019$IsNight <- ifelse(all_2019$Altitude < 0, 1, 0)


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
new_19 <- dplyr::select(rastered, -Raster_Values.ID)
colnames(new_19)[12] <- "Disturbance"

# here's how to see if times are missing!!
# filter(new_2019, stringr::str_length(Date_Time) < 11)

# all done! new_19 now has the deployment, observation, day/night binary, and human disturbance data for 2019!

write.csv(all_2019, "2019.csv", row.names=FALSE)

