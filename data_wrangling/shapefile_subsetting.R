# Shape File Subsetting
# Jesse Alston
# April 25, 2024

# PACKAGES ####
library(tidyverse)
library(overlap)
library(raster)
library(sf)
library(terra)
library(s2)


# DATA ####
all_ranges <- st_read('../data_too_big/POSSIBLY_USEFUL_MAMMALS')

# ANALYSIS ####
# subset wolf (x), puma (x), black bear (x), grizzly bear (x), coyote (x),
# white-tailed deer (x), mule deer (x), moose (x), elk (x)
# for coyote: whitetail (x), mule deer (x), all cottontails (silvilagus) (x), jackrabbits white (x), jackrabbits black (x), snowshoe hare (x)
# mesocarnivores: bobcat (x), opossum (x), raccoon(x), red fox(x), skunk (x)
range <- terra::subset(all_ranges,
                       all_ranges$sci_name == "Vulpes vulpes")

st_is_valid(range, reason=TRUE) # check if valid

# # if not valid
# range <- st_make_valid(range)
# st_is_valid(range, reason=TRUE) # check if valid again

sf_use_s2(TRUE)

# if multiple portions
range <- st_union(range) # merge disparate range portions
st_is_valid(range) # check to see if this worked (if so, will return just one TRUE)

s2::s2_rebuild(range) # rebuild with spherical geometry

# if the ggplot just loads and loads and never plots
range <- st_simplify(range, preserveTopology = FALSE, dTolerance = 1000)

# # if s2_rebuild fails
# range <- terra::subset(all_ranges, all_ranges$sci_name == "Canis latrans") # reload data
# range <- st_transform(range, crs=9822) # transform object to projected coordinates
# st_crs(range)$units # check units - should be meters
# range <- st_buffer(range, 1) # if meters, buffer by 1 m
# range <- st_transform(range, crs=4326) # transform back to original CRS
# range <- st_union(range) # merge again (this takes forever but does work eventually)
# s2::s2_rebuild(range) # rebuild again

sf_use_s2(FALSE)

# plot to check 
ggplot() +
  geom_sf(data = range, size = 1.5, color = "black", fill = "#0075C4", alpha = 0.5) +
  coord_sf()

# write file
st_write(range, "data/subset_shape_files/Vulpes_vulpes.shp")
