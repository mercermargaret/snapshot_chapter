# Wolf and Skunk Overlap Analysis
# Margaret Mercer
# May 22, 2024


library(tidyverse)
library(overlap)
library(raster)
library(sf)
library(terra)

# Range Subsetting ####

# get range data
prey_range <- st_read('data/subset_shape_files/Striped Skunk')
pred_range <- st_read('data/subset_shape_files/New_Wolf')
st_is_valid(prey_range, reason=TRUE)
st_is_valid(pred_range, reason=TRUE)

# switch off spherical geometry
sf_use_s2(FALSE)

# now calculate overlap
range_overlap <- st_intersection(prey_range, pred_range)
st_is_valid(range_overlap, reason=TRUE)

# let's mess with range_overlap until it gets normal again x_x
# rebuild ( didn't fix it )
# range_overlap <- st_make_valid(range_overlap)
# 
# # simplify?
# range_overlap <- st_union(range_overlap)

# if s2_rebuild fails
range_overlap <- st_transform(range_overlap, crs=9822) # transform object to projected coordinates
st_crs(range_overlap)$units # check units - should be meters
range_overlap <- st_buffer(range_overlap, 1) # if meters, buffer by 1 m
range_overlap <- st_transform(range_overlap, crs=4326) # transform back to original CRS
range_overlap <- st_union(range_overlap) # merge again (this takes forever but does work eventually)
s2::s2_rebuild(range_overlap) # rebuild again

# Convert df to an sf object
data <- read_csv("../data_too_big/all_years.csv")
points_sf <- st_make_valid(st_as_sf(data, coords = c("Longitude", "Latitude"), crs = st_crs(prey_range)))
st_is_valid(points_sf, reason=TRUE)

sf_use_s2(TRUE)

# Perform the point-in-polygon test
inside <- st_within(points_sf, range_overlap, sparse = FALSE)

# SOMETHING's off with the range overlap object. The other objects and the spatial points are fine and normal

# Extract rows from df that are inside the polygon
df_inside <- data[which(inside[,1]),]

# df_inside now contains only the rows where coordinates fall inside the polygon

# now make df_inside spatial
spatial_inside <- st_make_valid(st_as_sf(df_inside, coords = c("Longitude", "Latitude"), crs = st_crs(prey_range)))

## double check that this works by visualizing on map
ggplot() +
  geom_sf(data = prey_range, size = 1.5, color = "black", fill = "#0075C4", alpha = 0.5) +
  geom_sf(data = pred_range, size = 1.5, color = "black", fill = "#CB429F", alpha = 0.5) +
  geom_sf(data = range_overlap, size = 1.5, color = "black", fill = "#690375") +
  ggtitle("Predator/Prey Overlap") +
  geom_sf(data = spatial_inside) +
  coord_sf()

# Overlap Analysis ####

# subset to the two species
pair <- filter(df_inside, Species_Name == 'Canis lupus' | Species_Name == 'Mephitis mephitis')
# find median and assign to an object
median <- median(pair$Humans_Per_Camera_Per_Day)

# filter to low human disturbance
low_dist <- filter(df_inside, Humans_Per_Camera_Per_Day < median)

# convert time into radians
time <- as.POSIXct(low_dist$Local_Time, format = "%H:%M:%S")

# Extract hours, minutes, and seconds
hours <- as.numeric(format(time, "%H"))
minutes <- as.numeric(format(time, "%M"))
seconds <- as.numeric(format(time, "%S"))

# Convert time to radians
time_radians <- 2 * pi * ((hours + minutes / 60 + seconds / 3600) / 24)

# plot pred and prey for low disturbance
low_pred <- time_radians[low_dist$Species_Name == 'Canis lupus']
low_prey <- time_radians[low_dist$Species_Name == 'Mephitis mephitis']

pdf("results/overlap_graphs/wolf_skunk_low.pdf")
overlapPlot(low_pred, low_prey)
legend('topright', c("Predator", "Prey"), lty=c(1,2), col=c(1,4), bty='n')
dev.off()

overlap_low <- overlapEst(low_pred, low_prey, type="Dhat4")
overlap_low

# calculate bootstrap values
bootstrap_low <- bootstrap(low_pred, low_prey, 200, smooth=TRUE, kmax=3, adjust=NA, n.grid=128, type="Dhat4", cores=1)
# now calculate confidence intervals
bootCI(overlap_low, bootstrap_low, conf = 0.95)

## plot pred and prey overlap for HIGH disturbance

high_dist <- filter(df_inside, Humans_Per_Camera_Per_Day >= median)

# convert time into radians
time <- as.POSIXct(high_dist$Local_Time, format = "%H:%M:%S")


# Extract hours, minutes, and seconds
hours <- as.numeric(format(time, "%H"))
minutes <- as.numeric(format(time, "%M"))
seconds <- as.numeric(format(time, "%S"))

# Convert time to radians
time_radians <- 2 * pi * ((hours + minutes / 60 + seconds / 3600) / 24)

# plot pred and prey for high disturbance
high_pred <- time_radians[high_dist$Species_Name == 'Canis lupus']
high_prey <- time_radians[high_dist$Species_Name == 'Mephitis mephitis']

pdf("results/overlap_graphs/wolf_skunk_high.pdf")
overlapPlot(high_pred, high_prey)
legend('topright', c("Predator", "Prey"), lty=c(1,2), col=c(1,4), bty='n')
dev.off()


overlap_high <- overlapEst(high_pred, high_prey, type="Dhat4")
overlap_high

# calculate bootstrap values
bootstrap_high <- bootstrap(high_pred, high_prey, 200, smooth=TRUE, kmax=3, adjust=NA, n.grid=128, type="Dhat4", cores=1)
# now calculate confidence intervals
bootCI(overlap_high, bootstrap_high, conf = 0.95)


t.test(bootstrap_low, bootstrap_high, var.equal = TRUE)

# plot histogram of bootstrap values?
hist(bootstrap_low)
hist(bootstrap_high)

# t test
# data:  bootstrap_low and bootstrap_high
# t = -2.7617, df = 398, p-value = 0.006016
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#   -0.025648772 -0.004317283
# sample estimates:
#   mean of x mean of y 
# 0.5403266 0.5553097