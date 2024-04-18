# Puma and Mule Deer Overlap Analysis
# Margaret Mercer
# April 16, 2024


library(tidyverse)
library(overlap)
library(raster)
library(sf)
library(terra)

# Range Subsetting ####

# get range data
ranges <- st_read('data/POSSIBLY_USEFUL_MAMMALS')

# make a shape file for each species I'm interested in
# subset in R, write shape file into folder I want it in, then read those shape 
# files back into R when I need them

# subset
prey_range <- st_make_valid(terra::subset(ranges, ranges$sci_name == "Odocoileus hemionus"))
pred_range <- st_make_valid(terra::subset(ranges, ranges$sci_name == "Puma concolor"))

# do this once I've extracted individual species ranges
rm(ranges)
gc()

# unify muledeer
prey_range_unified <- st_make_valid(st_union(prey_range))

# now calculate overlap (when you unify a range, you need to place it in the second position of the intersection function for some reason)
range_overlap <- st_intersection(pred_range, prey_range_unified)

# Convert df to an sf object
data <- read_csv("data/all_years.csv")
points_sf <- st_make_valid(st_as_sf(data, coords = c("Longitude", "Latitude"), crs = st_crs(prey_range)))

# Perform the point-in-polygon test
inside <- st_within(points_sf, range_overlap, sparse = FALSE)

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


# Overlap Analysis ####

# filter to low human disturbance
low_dist <- filter(df_inside, Humans_Per_Camera_Per_Day < 0.1)

# convert time into radians
time <- as.POSIXct(low_dist$Local_Time, format = "%H:%M:%S")

# Extract hours, minutes, and seconds
hours <- as.numeric(format(time, "%H"))
minutes <- as.numeric(format(time, "%M"))
seconds <- as.numeric(format(time, "%S"))

# Convert time to radians
time_radians <- 2 * pi * ((hours + minutes / 60 + seconds / 3600) / 24)

# plot pred and prey for low disturbance
low_pred <- time_radians[low_dist$Species_Name == 'Puma concolor']
low_prey <- time_radians[low_dist$Species_Name == 'Odocoileus hemionus']

overlapPlot(low_pred, low_prey)
legend('topright', c("Predator", "Prey"), lty=c(1,2), col=c(1,4), bty='n')

overlap_low <- overlapEst(low_pred, low_prey, type="Dhat4")
overlap_low

# calculate bootstrap values
bootstrap_low <- bootstrap(low_pred, low_prey, 200, smooth=TRUE, kmax=3, adjust=NA, n.grid=128, type="Dhat4", cores=1)
# now calculate confidence intervals
bootCI(overlap_low, bootstrap_low, conf = 0.95)

## plot pred and prey overlap for HIGH disturbance

high_dist <- filter(df_inside, Humans_Per_Camera_Per_Day > 0.1)

# convert time into radians
time <- as.POSIXct(high_dist$Local_Time, format = "%H:%M:%S")


# Extract hours, minutes, and seconds
hours <- as.numeric(format(time, "%H"))
minutes <- as.numeric(format(time, "%M"))
seconds <- as.numeric(format(time, "%S"))

# Convert time to radians
time_radians <- 2 * pi * ((hours + minutes / 60 + seconds / 3600) / 24)

# plot pred and prey for high disturbance
high_pred <- time_radians[high_dist$Species_Name == 'Puma concolor']
high_prey <- time_radians[high_dist$Species_Name == 'Odocoileus hemionus']

overlapPlot(high_pred, high_prey)
legend('topright', c("Predator", "Prey"), lty=c(1,2), col=c(1,4), bty='n')

overlap_high <- overlapEst(high_pred, high_prey, type="Dhat4")
overlap_high

# calculate bootstrap values
bootstrap_high <- bootstrap(high_pred, high_prey, 200, smooth=TRUE, kmax=3, adjust=NA, n.grid=128, type="Dhat4", cores=1)
# now calculate confidence intervals
bootCI(overlap_high, bootstrap_high, conf = 0.95)


t.test(bootstrap_low, bootstrap_high, var.equal = TRUE)
# not as much of a difference 

# plot histogram of bootstrap values?
hist(bootstrap_low)
hist(bootstrap_high)
# yes they're normal!:)

# Puma: strong shift to nocturnality with high human presence
# Mule deer: strong crepuscular peaks flatten with higher human disturbance; noct increases slightly
# Overlap: greater overlap in areas of low human disturbance than high (p < 0.01)

