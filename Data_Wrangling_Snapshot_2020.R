# Data Wrangling Snapshot 2020
# (same thing as done in "Data_Wrangling_Snapshot_2019"; that document contains explanatory comments)
# Margaret Mercer
# December 15, 2023

library(dplyr)
library(suncalc)

obs_2020 <- read.csv("SNAPSHOT_USA_2020_observations.csv")
dep_2020 <- read.csv("SNAPSHOT_USA_2020_deployments.csv")

## all over again, with 2020 data
# not all have deployment IDs, so had to merge by site name!

left_joined_20 <- merge(obs_2020, dep_2020, by = "Site_Name", all.x = TRUE)
all_2020 <- left_joined_20[, c("Camera_Trap_Array.x", "Site_Name", "Survey_Days", "Latitude", "Longitude", "Begin.Time", "Species_Name", "Common_Name", "Count")]

colnames(all_2020)[4] <- "lat"
colnames(all_2020)[5] <- "lon"
colnames(all_2020)[6] <- "date"

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
colnames(all_2020)[6] <- "Date_Time"

all_2020$IsNight <- ifelse(all_2020$Altitude < 0, 1, 0)


library(terra)
library(sf)

full_raster <- rast("/Users/mmercer3/Downloads/ml-hfi_v1_2019.tif.crdownload")
max(full_raster)

# crop dataset from the whole world to just the US

e <- extent(-167, -66, 20, 72)
raster <- terra::crop(full_raster, e)
plot(raster) # cut off at 70* N but that's ok because none of our observations are futher north than that

# associate lats and longs with dataframe

sf_points <- st_as_sf(all_2020, coords = c("Longitude", "Latitude"), crs = 4326)
values_at_points <- extract(raster, sf_points)
rastered <- cbind(all_2020, Raster_Values = values_at_points)
new_20 <- dplyr::select(rastered, -Raster_Values.ID)
colnames(new_20)[12] <- "Disturbance"

# Cool. new_20 now has the deployment, observation, day/night binary, and human disturbance data for 2020!
# You can do this for any year of SnapShotUSA data; simply replace the year number in each of the objects and check lat/long to make sure it still all falls within the range

write.csv(new_20, "2020.csv", row.names=FALSE)
