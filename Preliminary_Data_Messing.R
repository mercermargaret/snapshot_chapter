# Preliminary Data Messing
# (partner document is "Test_Data_Wrangling.R")
# Margaret Mercer
# Dec 11, 2023

obs_2019 <- read.csv("SNAPSHOT_USA_2019_observations.csv")
dep_2019 <- read.csv("SNAPSHOT_USA_2019_deployments.csv")
obs_2020 <- read.csv("SNAPSHOT_USA_2020_observations.csv")
dep_2020 <- read.csv("SNAPSHOT_USA_2020_deployments.csv")

# merge deployment and observation data
library(dplyr)

left_joined_19 <- merge(obs_2019, dep_2019, by = "Deployment_ID", all.x = TRUE)
all_2019 <- left_joined_19[, c("Deployment_ID", "Site_Name.x", "Survey_Days", "Latitude.x", "Longitude.x", "Begin_Time", "Species_Name", "Count")]
# Ok now we've included only the columns we want.

# Code a binary for day/night observation
# install.packages("suncalc")
library(suncalc)

# rename columns so suncalc can recognize them
colnames(all_2019)[4] <- "lat"
colnames(all_2019)[5] <- "lon"
colnames(all_2019)[6] <- "date"

#fix date format so suncalc can do its thang
formatted_time <- as.POSIXct(strptime(all_2019$date, format = "%m/%d/%y %H:%M"))

all_2019$date <- formatted_time

sun_position <- getSunlightPosition(
  data = all_2019,
  keep = c("altitude")
)

all_2019['Altitude'] = sun_position$altitude

# let's rename some columns so they look nice
colnames(all_2019)[2] <- "Site_Name"
colnames(all_2019)[4] <- "Latitude"
colnames(all_2019)[5] <- "Longitude"
colnames(all_2019)[6] <- "Date_Time"

# now let's code 1s and 0s for altitude (sun position) (1 for night observation, 0 for day observation)
# so we want a 1 if it's negative, and a 0 if it's positive
all_2019$IsNight <- ifelse(all_2019$Altitude < 0, 1, 0)


## all over again, with 2020 data
# not all have deployment IDs, so had to merge by site name!

left_joined_20 <- merge(obs_2020, dep_2020, by = "Site_Name", all.x = TRUE)
all_2020 <- left_joined_20[, c("Deployment_ID.x", "Site_Name", "Survey_Days", "Latitude", "Longitude", "Begin.Time", "Species_Name", "Count")]

colnames(all_2020)[4] <- "lat"
colnames(all_2020)[5] <- "lon"
colnames(all_2020)[6] <- "date"

# date and time already in correct format!

sun_position <- getSunlightPosition(
  data = all_2020,
  keep = c("altitude")
)

all_2020['Altitude'] = sun_position$altitude

colnames(all_2020)[1] <- "Deployment_ID"
colnames(all_2020)[2] <- "Site_Name"
colnames(all_2020)[4] <- "Latitude"
colnames(all_2020)[5] <- "Longitude"
colnames(all_2020)[6] <- "Date_Time"

all_2020$IsNight <- ifelse(all_2020$Altitude < 0, 1, 0)


# Now let's think about human presence and how we're going to measure that. I'll need that map from Jesse and get human disturbance values from that
# so I think I need the usgs human disturbance dataset (listing areas of high, medium, and low avoidance (basically areas of high avoidance are places with low human avoidance))
# let's load the terra package

# install.packages("terra")
library(terra)
library(sf)

# now i'm going to try to load the datasets
gp_raster <- rast("/Users/mmercer3/Downloads/snapshot_chapter/avoid_gp/avoid_gp")
ne_raster <- rast("/Users/mmercer3/Downloads/snapshot_chapter/avoid_ne/avoid_ne")
nw_raster <- rast("/Users/mmercer3/Downloads/snapshot_chapter/avoid_nw/avoid_nw")
se_raster <- rast("/Users/mmercer3/Downloads/snapshot_chapter/avoid_se/avoid_se")
sw_raster <- rast("/Users/mmercer3/Downloads/snapshot_chapter/avoid_sw/avoid_sw")
um_raster <- rast("/Users/mmercer3/Downloads/snapshot_chapter/avoid_um/avoid_um")

sf_points <- st_as_sf(all_2019, coords = c("Longitude", "Latitude"), crs = 4326)

values_at_points_gp <- extract(gp_raster, sf_points)
values_at_points_ne <- extract(ne_raster, sf_points)
values_at_points_nw <- extract(nw_raster, sf_points)
values_at_points_se <- extract(se_raster, sf_points)
values_at_points_sw <- extract(sw_raster, sf_points)
values_at_points_um <- extract(um_raster, sf_points)

final_df <- cbind(all_2019, Raster_Values1 = values_at_points_gp, 
                  Raster_Values2 = values_at_points_ne, 
                  Raster_Values3 = values_at_points_nw, 
                  Raster_Values4 = values_at_points_se, 
                  Raster_Values5 = values_at_points_sw, 
                  Raster_Values6 = values_at_points_um)

# so I don't know what the 0s mean...does that mean human presence? Will there even be enough values that 
# aren't 0 for this to be useful? I need to figure out what 0-3 means. And how to collapse all rasters in 
# the dataframe into one raster value column, rather than a whole bunch.


trimmed <- select(final_df, -Raster_Values1.ID, 
                  -Raster_Values2.ID, 
                  -Raster_Values3.ID, 
                  -Raster_Values4.ID, 
                  -Raster_Values5.ID, 
                  -Raster_Values6.ID)

trimmed$Raster_Value <- coalesce(trimmed$Raster_Values1.avoid_gp, 
                                 trimmed$Raster_Values2.avoid_ne, 
                                 trimmed$Raster_Values3.avoid_nw, 
                                 trimmed$Raster_Values4.avoid_se, 
                                 trimmed$Raster_Values5.avoid_sw, 
                                 trimmed$Raster_Values6.avoid_um)

# now there's one column that coalesced all the values into one column, effectively getting rid of all the NAs
# so we can delete all the old raster values columns

new_19 <- select(trimmed, -Raster_Values1.avoid_gp, 
                 -Raster_Values2.avoid_ne, 
                 -Raster_Values3.avoid_nw, 
                 -Raster_Values4.avoid_se, 
                 -Raster_Values5.avoid_sw, 
                 -Raster_Values6.avoid_um)

# now I need to check with Jesse that this is actually gonna work, and that I did it right...like is this even the dataset he suggested? Idk


