# human disturbance index troubleshooting
# margaret mercer
# july 29
# figure out what's going on and why we have so many NAs in our human disturbance index
# nevermind, were not gonna use disturbance after all

library(terra)
library(sf)
library(ggplot2)

# this hggplot2# this has always been a problem
old2020 <- read.csv("data/four_year_data/2019.csv")
old2019 <- read.csv("data/four_year_data/2019.csv")
old2020 <- read.csv("data/four_year_data/2020.csv")
old2021 <- read.csv("data/four_year_data/2021.csv")
old2022 <- read.csv("data/four_year_data/2022.csv")
sum(is.na(old2019$Disturbance))
sum(is.na(old2020$Disturbance))
sum(is.na(old2021$Disturbance))
sum(is.na(old2022$Disturbance))

data <- read.csv("../data_too_big/five_year_observation_data.csv")
deployments <- read.csv("data/five_year_deployments.csv")

# # heres the code to add disturbance:
# # add column for human disturbance index
# full_raster <- rast("/Users/mmercer3/Downloads/ml-hfi_v1_2019.tif.crdownload")
# max(full_raster)
# # crop dataset from the whole world to just the US:
# e <- extent(-167, -66, 20, 72)
# raster <- terra::crop(full_raster, e)
# plot(raster) # to be sure it actually includes US like we want it to. 
# # Looks like Alaska is slightly cut off (at 70*) but our highest lat is only 59* so that's ok
# # associate lats and longs with dataframe:
# sf_points <- st_as_sf(data, coords = c("Longitude", "Latitude"), crs = 4326)
# values_at_points <- terra::extract(raster, sf_points)
# rastered <- cbind(data, Raster_Values = values_at_points)
# data <- dplyr::select(rastered, -Raster_Values.ID)
# data <- rename(data, Disturbance = `Raster_Values.ml-hfi_v1_2019.tif`)


# plot data to see how disturbance and human activity vary
hist(deployments$Disturbance)
hist(log(deployments$Humans_Per_Camera_Per_Day))

ggplot(data = deployments, aes(x = Disturbance, y = Humans_Per_Camera_Per_Day, color = Development_Level)) +
  geom_point(alpha = 0.25)
