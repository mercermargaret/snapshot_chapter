# Figuring out Range Raster Data
# Margaret Mercer
# April 4, 2024

library(raster)
library(sf)
library(terra)
library(tidyverse)

# do this once I've extracted all the individual species ranges I want
# rm(ranges)
# gc()

# get range data
ranges <- st_read('../data_too_big/MAMMALS_TERRESTRIAL_ONLY')
head(ranges)
ranges

# make a shape file for each species I'm interested in
# subset in R, write shape file into folder I want it in, then read those shape 
# files back into R when I need them

# subset
whiterange <- st_make_valid(terra::subset(ranges, ranges$sci_name == "Odocoileus virginianus"))
pumarange <- st_make_valid(terra::subset(ranges, ranges$sci_name == "Puma concolor"))
# mulerange <- terra::subset(ranges, ranges$sci_name == "Odocoileus hemionus")

# now overlap
wpoverlap <- st_intersection(whiterange, pumarange)
# gives me this warning: attribute variables are assumed to be spatially constant throughout all geometries



# from chatgpt:

# Assuming df is your dataframe with 'latitude' and 'longitude' columns
# Convert df to an sf object
dataframe <- read_csv("data/all_years.csv")
points_sf <- st_make_valid(st_as_sf(dataframe, coords = c("Longitude", "Latitude"), crs = st_crs(whiterange)))

# Perform the point-in-polygon test
inside <- st_within(points_sf, wpoverlap, sparse = FALSE)
# this did not work!!!! because:
# Error in wk_handle.wk_wkb(wkb, s2_geography_writer(oriented = oriented,  : 
# Loop 2 is not valid: Edge 68867 crosses edge 68870
# AND later, when I re-ran it, it gave me this:
# Error in wk_handle.wk_wkb(wkb, s2_geography_writer(oriented = oriented,  : 
# Loop 9 is not valid: Edge 1151 is degenerate (duplicate vertex)

# Extract rows from df that are inside the polygon
df_inside <- dataframe[which(inside),]

# df_inside now contains only the rows where coordinates fall inside the polygon

# now make df_inside spatial
spatial_inside <- st_make_valid(st_as_sf(df_inside, coords = c("Longitude", "Latitude"), crs = st_crs(whiterange)))

# data visualization
library(ggplot2)
ggplot() +
  geom_sf(data = whiterange, size = 1.5, color = "black", fill = "#8E518D") +
  ggtitle("White Tail Deer Range") +
  coord_sf()

ggplot() +
  geom_sf(data = pumarange, size = 1.5, color = "black", fill = "#8E518D") +
  ggtitle("Mountain Lion Range") +
  coord_sf()

# cool!!!!

ggplot() +
  geom_sf(data = whiterange, size = 1.5, color = "black", fill = "#5DD9C1", alpha = 0.5) +
  geom_sf(data = pumarange, size = 1.5, color = "black", fill = "#CB429F", alpha = 0.5) +
  geom_sf(data = wpoverlap[1,], size = 1.5, color = "black", fill = "#690375") +
  ggtitle("Puma/Whitetail Overlap") +
  geom_sf(data = spatial_inside) +
  coord_sf()


# cut down original range data to a smaller, more manageable dataset
ranges <- ranges[ranges$order_!="CHIROPTERA",]
ranges <- ranges[ranges$order_!="PRIMATES",]
ranges <- ranges[ranges$order_!="AFROSORICIDA",]
ranges <- ranges[ranges$order_!="EULIPOTYPHLA",]
ranges <- ranges[ranges$order_!="HYRACOIDEA",]
ranges <- ranges[ranges$order_!="TUBULIDENTATA",]
ranges <- ranges[ranges$order_!="MONOTREMATA",]

st_write(ranges, '..data_too_big/POSSIBLY_USEFUL_MAMMALS/ranges.shp')