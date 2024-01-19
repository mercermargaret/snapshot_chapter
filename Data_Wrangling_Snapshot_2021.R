# Data Wrangling Snapshot 2021
# Margaret Mercer
# January 12, 2024

library(dplyr)
library(suncalc)

obs_2021 <- read.csv("ssusa_2021_sequences.csv")
dep_2021 <- read.csv("ssusa_2021_deployments.csv")

left_joined_21 <- merge(obs_2021, dep_2021, by = "deployment_id", all.x = TRUE)
all_2021 <- left_joined_21[, c("subproject_name", "deployment_id", "start_date", "end_date",  "latitude", "longitude", "start_time", "species_name", "common_name", "group_size")] #"subproject_name" is comparable to "array" in 2019 and 2020 data

# subtract start date from end date to get "Survey_Days" into ONE column, rather than two
Survey_Days <- as.Date(all_2021$end_date) - as.Date(all_2021$start_date)
Survey_Days <- as.numeric(gsub("[^0-9]+", "", Survey_Days))
all_2021$Survey_Days <- Survey_Days
all_2021 <- all_2021[, c("subproject_name", "deployment_id", "Survey_Days",  "latitude", "longitude", "start_time", "species_name", "common_name", "group_size")]

colnames(all_2021)[4] <- "lat"
colnames(all_2021)[5] <- "lon"
colnames(all_2021)[6] <- "date"

# fix date and time formatting
formatted_time <- as.POSIXct(all_2021$date, format = "%m/%d/%Y %H:%M:%S")

all_2021$date <- formatted_time

sun_position <- getSunlightPosition(
  data = all_2021,
  keep = c("altitude")
)

all_2021['Altitude'] = sun_position$altitude

colnames(all_2021)[1] <- "Array"
colnames(all_2021)[2] <- "Site_Name"
colnames(all_2021)[4] <- "Latitude"
colnames(all_2021)[5] <- "Longitude"
colnames(all_2021)[6] <- "Date_Time"
colnames(all_2021)[7] <- "Species_Name"
colnames(all_2021)[8] <- "Common_Name"
colnames(all_2021)[9] <- "Count"

all_2021$IsNight <- ifelse(all_2021$Altitude < 0, 1, 0)

library(terra)
library(sf)

full_raster <- rast("/Users/mmercer3/Downloads/ml-hfi_v1_2019.tif.crdownload")
max(full_raster)

# crop dataset from the whole world to just the US

e <- extent(-167, -66, 20, 72)
raster <- terra::crop(full_raster, e)
plot(raster) # cut off at 70* N but that's ok because none of our observations are futher north than that

# associate lats and longs with dataframe

sf_points <- st_as_sf(all_2021, coords = c("Longitude", "Latitude"), crs = 4326)
values_at_points <- extract(raster, sf_points)
rastered <- cbind(all_2021, Raster_Values = values_at_points)
new_21 <- dplyr::select(rastered, -Raster_Values.ID)
colnames(new_21)[12] <- "Disturbance"

# Cool. new_21 now has the deployment, observation, day/night binary, and human disturbance data for 2021!
# You can do this for any year of SnapShotUSA data; simply replace the year number in each of the objects and check lat/long to make sure it still all falls within the range

write.csv(new_21, "2021.csv", row.names=FALSE)