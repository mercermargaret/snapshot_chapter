
library(tidyverse)
library(overlap)
library(raster)
library(sf)
library(terra)

# ok so the PROBLEM is that the BEAR range is split? And so we have to combine it?? and THAT makes it so it doesn't work :') Same thing happening in grizzly-elk (even though it didnt happen when I first ran it)

# get range data
ranges <- st_read('../data_too_big/POSSIBLY_USEFUL_MAMMALS')

# subset
prey_range <- st_make_valid(terra::subset(ranges, ranges$sci_name == "Alces alces"))
pred_range <- st_make_valid(terra::subset(ranges, ranges$sci_name == "Ursus arctos"))


sf_use_s2(FALSE)

# cut out right half of both ranges because there's a point there causing problems when we try to get overlap
# Creating a rectangular box from -180 to 0 longitude (covering all latitudes)
meridian_box <- st_sfc(st_polygon(list(
  rbind(c(-180, -90), c(0, -90), c(0, 90), c(-180, 90), c(-180, -90))
)), crs = 4326)
# Intersect the polygon with the box to keep only parts with negative longitudes
prey_range <- st_intersection(prey_range, meridian_box)
pred_range <- st_intersection(pred_range, meridian_box)

# the following code is what I added that is making everything else break :')
pred_range_unified <- st_make_valid(st_union(pred_range))
range_overlap <- st_intersection(prey_range, pred_range_unified, dimension = "polygon")

# range_overlap <- st_intersection(prey_range, pred_range)

sf_use_s2(TRUE)

# Convert df to an sf object
data <- read_csv("data/all_years.csv")
points_sf <- st_make_valid(st_as_sf(data, coords = c("Longitude", "Latitude"), crs = st_crs(prey_range)))

# Perform the point-in-polygon test
inside <- st_within(points_sf, range_overlap, sparse = FALSE)
# Error in wk_handle.wk_wkb(wkb, s2_geography_writer(oriented = oriented,  : 
# Loop 6 is not valid: Edge 1299 is degenerate (duplicate vertex)

# OR if I change dimension to polygon in line 31, line 42 now gives me:
# Error in wk_handle.wk_wkb(wkb, s2_geography_writer(oriented = oriented,  : 
# Loop 3 is not valid: Edge 35585 crosses edge 35587

# when I add "st_make_valid" to line 42 it gives me:
# Error in wk_handle.wk_wkb(wkb, s2_geography_writer(oriented = oriented,  : 
# Loop 0 is not valid: Edge 32 crosses edge 34

# the internet says the solution is to switch off spherical geometry, but whenever I try to run 42 with spherical geometry off it just runs forever and never finishes
# OR it says:
# Error in scan(text = lst[[length(lst)]], quiet = TRUE) : 
#   scan() expected 'a real', got '51.293673399999996.'
# Error in (function (msg)  : 
#             TopologyException: side location conflict at -122.3375207 51.293673399999996. This can occur if the input geometry is invalid.

# st_make_valid doesn't help


# Extract rows from df that are inside the polygon
df_inside <- data[which(inside[,1]),]

# df_inside now contains only the rows where coordinates fall inside the polygon

# now make df_inside spatial
spatial_inside <- st_make_valid(st_as_sf(df_inside, coords = c("Longitude", "Latitude"), crs = st_crs(prey_range)))


# double check that this works by visualizing on map
ggplot() +
  geom_sf(data = prey_range, size = 1.5, color = "black", fill = "#0075C4", alpha = 0.5) +
  geom_sf(data = pred_range, size = 1.5, color = "black", fill = "#CB429F", alpha = 0.5) +
  geom_sf(data = range_overlap[1,], size = 1.5, color = "black", fill = "#690375") +
  ggtitle("Predator/Prey Overlap") +
  geom_sf(data = spatial_inside) +
  coord_sf()
