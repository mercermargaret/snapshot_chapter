# Overlap map for species range for illustration in paper
# (adapted from "range subsetting" portion of overlap analysis document)
# Margaret Mercer
# June 14, 2024

library(tidyverse)
library(overlap)
library(raster)
library(sf)
library(terra)
library(rnaturalearthdata)
library(rnaturalearth)

# get range data
prey_range <- st_read('data/subset_shape_files/Whitetail')
pred_range <- st_read('data/subset_shape_files/Puma')
st_is_valid(prey_range, reason=TRUE)
st_is_valid(pred_range, reason=TRUE)

# switch off spherical geometry
sf_use_s2(FALSE)

# now calculate overlap
range_overlap <- st_intersection(prey_range, pred_range)
st_is_valid(range_overlap, reason=TRUE)

# Trim df for duplicate points
data <- read_csv("../data_too_big/all_years.csv")
# data <- arrange(data, Latitude)
data_new <- data %>% mutate(across(c('Latitude', 'Longitude'), round, 3))
trimmed <- data_new %>%
  group_by(Array) %>%
  slice(1) %>%
  ungroup()
trimmed <- trimmed %>%
  distinct_at("Latitude", .keep_all = TRUE)
trimmed <- trimmed %>%
  distinct_at("Longitude", .keep_all = TRUE)
# trimming like above cuts off the point in florida

# convert to an sf object
points_sf <- st_make_valid(st_as_sf(trimmed, coords = c("Longitude", "Latitude"), crs = st_crs(prey_range)))

sf_use_s2(TRUE)

# Perform the point-in-polygon test
inside <- st_within(points_sf, range_overlap, sparse = FALSE)

# Extract rows from df that are inside the polygon
df_inside <- trimmed[which(inside[,1]),]

# df_inside now contains only the rows where coordinates fall inside the polygon

# now make df_inside spatial for visualization
spatial_inside <- st_make_valid(st_as_sf(df_inside, coords = c("Longitude", "Latitude"), crs = st_crs(prey_range)))

# make map
# ggplot() +
#   geom_sf(data = prey_range, size = 1.5, color = "black", fill = "#0075C4", alpha = 0.5) +
#   geom_sf(data = pred_range, size = 1.5, color = "black", fill = "#CB429F", alpha = 0.5) +
#   geom_sf(data = range_overlap[1,], size = 1.5, color = "black", fill = "#690375") +
#   ggtitle("Predator/Prey Overlap") +
#   geom_sf(data = points_sf, color = "gray", size = 1) +
#   geom_sf(data = spatial_inside, color = "black") +
#   theme_void() +
#   coord_sf()

# add US border
na <- ne_countries(continent = "North America", returnclass = "sf")
na <- filter(na, sovereignt != "Denmark")
sa <- ne_countries(continent = "South America", returnclass = "sf")
outline <- st_union(na, sa)

# ggplot() +
#   geom_sf(data = outline, fill = NA, color = "black")

# map with US border included
# ggplot() +
#   # geom_sf(data = outline, fill = NA, color = "#5D5D5D") +
#   geom_sf(data = prey_range, size = 1.5, color = "black", fill = "#0075C4", alpha = 0.5) +
#   geom_sf(data = pred_range, size = 1.5, color = "black", fill = "#CB429F", alpha = 0.5) +
#   geom_sf(data = range_overlap[1,], size = 1.5, color = "black", fill = "#690375") +
#   # geom_sf(data = points_sf, color = "black", size = 0.5) +
#   geom_sf(data = spatial_inside, color = "black", size = 2, pch = 18) +
#   ggtitle("Predator/Prey Overlap") +
#   theme_classic() +
#   coord_sf()
# 
# ggsave("species_range_overlap_map5.png")


# OK I'm gonna try something crazy
# I'm gonna try to crop the range maps down to just the US
# wish me luck :')

# so we want intersection between prey_range and the united states

# so let's grab the US
usa <- ne_states(country = "United States of America", returnclass = "sf")

# and find the intersection
us_prey <- st_intersection(prey_range, usa)

# test to see if it worked .... YES it did:)
# ggplot() +
#   geom_sf(data = us_prey, size = 1.5, color = NA, fill = "#0075C4", alpha = 0.5)

us_pred <- st_intersection(pred_range, usa)
# ggplot() +
#   geom_sf(data = us_pred, size = 1.5, color = NA, fill = "#CB429F", alpha = 0.5)

us_overlap <- st_intersection(range_overlap, usa)
# ggplot() +
#   geom_sf(data = us_overlap, size = 1.5, color = NA, fill = "#690375")


# map of JUST US with US border included
# ggplot() +
#   geom_sf(data = usa, fill = NA, color = "#5D5D5D") +
#   geom_sf(data = us_prey, size = 1.5, color = "black", fill = "#0075C4", alpha = 0.5) +
#   geom_sf(data = us_pred, size = 1.5, color = "black", fill = "#CB429F", alpha = 0.5) +
#   geom_sf(data = us_overlap, size = 1.5, color = "black", fill = "#690375") +
#   geom_sf(data = points_sf, color = "black", size = 0.5) +
#   geom_sf(data = spatial_inside, color = "black", size = 2, pch = 18) +
#   ggtitle("Predator/Prey Overlap") +
#   theme_classic() +
#   coord_sf()

# Invert the logical matrix to find points OUTSIDE the polygon
outside <- !inside[,1]

# Extract rows from df that are outside the polygon
df_outside <- trimmed[which(outside),]

# make it spatial
spatial_outside <- st_make_valid(st_as_sf(df_outside, coords = c("Longitude", "Latitude"), crs = st_crs(prey_range)))

# now fix alaska and hawaii
new_usa <- tigris::shift_geometry(
  usa,
  geoid_column = NULL,
  preserve_area = TRUE,
  position = "below"
)

new_cameras <- tigris::shift_geometry(
  points_sf,
  geoid_column = NULL,
  preserve_area = TRUE,
  position = "below"
)

new_pred <- tigris::shift_geometry(
  us_pred,
  geoid_column = NULL,
  preserve_area = TRUE,
  position = "below"
)

new_outside <- tigris::shift_geometry(
  spatial_outside,
  geoid_column = NULL,
  preserve_area = TRUE,
  position = "below"
)

# Warning message:
#   None of your features are in Alaska, Hawaii, or Puerto Rico, so no geometries will be shifted.
# Transforming your object's CRS to 'ESRI:102003'

# this just means you need to keep the "new_usa" as the first thing you plot in the ggplot to preserve area and shape of alaska

# # and plot with alaska and hawaii in the right places:)
# ggplot() +
#   geom_sf(data = new_usa, fill = NA, color = "black") +
#   geom_sf(data = us_prey, size = 1.5, color = "black", fill = "#0075C4", alpha = 0.5) +
#   geom_sf(data = new_pred, size = 1.5, color = "black", fill = "#CB429F", alpha = 0.5) +
#   geom_sf(data = us_overlap, size = 1.5, color = "black", fill = "#690375", alpha = 0.5) +
#   # geom_sf(data = spatial_inside, color = "black", size = 4) + # not sure about this line, lets just see. it lines the white with black again xD
#   geom_sf(data = spatial_inside, color = "white", size = 3) +
#   geom_sf(data = new_cameras, color = "black", size = 1) +
#   ggtitle("Predator/Prey Overlap") +
#   theme_classic() +
#   coord_sf()
# # ^^ white outline of black dot

ggplot() +
  geom_sf(data = new_usa, fill = NA, color = NA) +  
  geom_sf(data = us_prey, size = 1.5, color = "#4D4D4D", fill = "#0075C4", alpha = 0.5) +
  geom_sf(data = new_pred, size = 1.5, color = "#4D4D4D", fill = "#CB429F", alpha = 0.5) +
  geom_sf(data = us_overlap, size = 1.5, color = "#4D4D4D", fill = "#690375", alpha = 0.5) +
  geom_sf(data = new_usa, fill = NA, color = "#4D4D4D") +
  geom_sf(data = new_outside, fill = "black", color = "black", size = 3, alpha = 0.4) +
  geom_sf(data = spatial_inside, fill = "white", color = "black", size = 3, shape = 21, alpha = 0.75) +
  ggtitle("Predator/Prey Overlap") +
  theme_classic() +
  coord_sf()
# ^^ black outline of white dot

