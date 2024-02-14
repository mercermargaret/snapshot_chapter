# Data Wrangling Snapshot 2022
# Margaret Mercer
# January 12, 2024

library(dplyr)
library(suncalc)

obs_2022 <- read.csv("updated_2022_sequences.csv")
dep_2022 <- read.csv("updated_2022_deployments.csv")

left_joined_22 <- left_join(obs_2022, dep_2022, by = "deployment_id")
all_2022 <- left_joined_22[, c("subproject_name", "deployment_id", "survey_days",  "latitude", "longitude", "start_time", "genus", "species", "common_name", "group_size")] #"subproject_name" is comparable to "array" in 2019 and 2020 data

# put genus and species together to make a "species_name" column
all_2022$Species_Name <- paste(all_2022$genus, all_2022$species, sep = " ")
# if Species_Name is empty, replace it with common_name
all_2022$Species_Name <- ifelse(all_2022$Species_Name == "   ", all_2022$common_name, all_2022$Species_Name)
all_2022 <- dplyr::select(all_2022, -genus, -species)

colnames(all_2022)[4] <- "lat"
colnames(all_2022)[5] <- "lon"
colnames(all_2022)[6] <- "date"

# fix date and time formatting
formatted_time <- as.POSIXct(all_2022$date, format = "%Y/%m/%d %H:%M:%S")

all_2022$date <- formatted_time

sun_position <- getSunlightPosition(
  data = all_2022,
  keep = c("altitude")
)

all_2022['Altitude'] = sun_position$altitude

all_2022 <- all_2022[, c("subproject_name", "deployment_id", "survey_days", "lat", "lon", "date", "Species_Name", "common_name", "group_size", "Altitude")]

colnames(all_2022)[1] <- "Array"
colnames(all_2022)[2] <- "Site_Name"
colnames(all_2022)[3] <- "Survey_Days"
colnames(all_2022)[4] <- "Latitude"
colnames(all_2022)[5] <- "Longitude"
colnames(all_2022)[6] <- "Date_Time"
colnames(all_2022)[7] <- "Species_Name"
colnames(all_2022)[8] <- "Common_Name"
colnames(all_2022)[9] <- "Count"

all_2022$IsNight <- ifelse(all_2022$Altitude < 0, 1, 0)

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
values_at_points <- extract(raster, sf_points)
rastered <- cbind(all_2022, Raster_Values = values_at_points)
new_22 <- dplyr::select(rastered, -Raster_Values.ID)
colnames(new_22)[12] <- "Disturbance"

# Cool. new_22 now has the deployment, observation, day/night binary, and human disturbance data for 2022!
# You can do this for any year of SnapShotUSA data; simply replace the year number in each of the objects and check lat/long to make sure it still all falls within the range

write.csv(new_22, "2022.csv", row.names=FALSE)
