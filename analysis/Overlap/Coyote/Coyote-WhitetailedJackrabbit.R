# Coyote and Whitetailed Jackrabbit Overlap Analysis
# Margaret Mercer
# May 1, 2024

library(tidyverse)
library(overlap)
library(raster)
library(sf)
library(terra)

# Range Subsetting ####

# get range data
prey_range <- st_read('data/subset_shape_files/Whitetailed Jackrabbit')
pred_range <- st_read('data/subset_shape_files/Coyote')
st_is_valid(prey_range, reason=TRUE)
st_is_valid(pred_range, reason=TRUE)

# switch off spherical geometry
sf_use_s2(FALSE)

# now calculate overlap
range_overlap <- st_intersection(prey_range, pred_range)
st_is_valid(range_overlap, reason=TRUE)

# Convert df to an sf object
data <- read_csv("../data_too_big/all_years.csv")
points_sf <- st_make_valid(st_as_sf(data, coords = c("Longitude", "Latitude"), crs = st_crs(prey_range)))

sf_use_s2(TRUE)

# Perform the point-in-polygon test
inside <- st_within(points_sf, range_overlap, sparse = FALSE)

# Extract rows from df that are inside the polygon
df_inside <- data[which(inside[,1]),]

# df_inside now contains only the rows where coordinates fall inside the polygon

# now make df_inside spatial
spatial_inside <- st_make_valid(st_as_sf(df_inside, coords = c("Longitude", "Latitude"), crs = st_crs(prey_range)))

# if ggplot just takes forever to load
prey_range <- st_simplify(prey_range, preserveTopology = FALSE, dTolerance = 1000)
range_overlap <- st_simplify(range_overlap, preserveTopology = FALSE, dTolerance = 1000)
pred_range <- st_simplify(pred_range, preserveTopology = FALSE, dTolerance = 1000)

# double check that this works by visualizing on map
# ggplot() +
#   geom_sf(data = prey_range, size = 1.5, color = "black", fill = "#0075C4", alpha = 0.5) +
#   geom_sf(data = pred_range, size = 1.5, color = "black", fill = "#CB429F", alpha = 0.5) +
#   geom_sf(data = range_overlap[1,], size = 1.5, color = "black", fill = "#690375") +
#   ggtitle("Predator/Prey Overlap") +
#   geom_sf(data = spatial_inside) +
#   coord_sf()

# Overlap Analysis ####

# filter to low human disturbance
low_dist <- filter(df_inside, Humans_Per_Camera_Per_Day < median(data$Humans_Per_Camera_Per_Day))

# convert time into radians
time <- as.POSIXct(low_dist$Local_Time, format = "%H:%M:%S")

# Extract hours, minutes, and seconds
hours <- as.numeric(format(time, "%H"))
minutes <- as.numeric(format(time, "%M"))
seconds <- as.numeric(format(time, "%S"))

# Convert time to radians
time_radians <- 2 * pi * ((hours + minutes / 60 + seconds / 3600) / 24)

# plot pred and prey for low disturbance
low_pred <- time_radians[low_dist$Species_Name == 'Canis latrans']
low_prey <- time_radians[low_dist$Species_Name == 'Lepus townsendii']

overlapPlot(low_pred, low_prey)
legend('topright', c("Predator", "Prey"), lty=c(1,2), col=c(1,4), bty='n')

overlap_low <- overlapEst(low_pred, low_prey, type="Dhat4")
overlap_low

# calculate bootstrap values
bootstrap_low <- bootstrap(low_pred, low_prey, 200, smooth=TRUE, kmax=3, adjust=NA, n.grid=128, type="Dhat4", cores=1)
# now calculate confidence intervals
bootCI(overlap_low, bootstrap_low, conf = 0.95)

## plot pred and prey overlap for HIGH disturbance

high_dist <- filter(df_inside, Humans_Per_Camera_Per_Day >= median(data$Humans_Per_Camera_Per_Day))

# convert time into radians
time <- as.POSIXct(high_dist$Local_Time, format = "%H:%M:%S")


# Extract hours, minutes, and seconds
hours <- as.numeric(format(time, "%H"))
minutes <- as.numeric(format(time, "%M"))
seconds <- as.numeric(format(time, "%S"))

# Convert time to radians
time_radians <- 2 * pi * ((hours + minutes / 60 + seconds / 3600) / 24)

# plot pred and prey for high disturbance
high_pred <- time_radians[high_dist$Species_Name == 'Canis latrans']
high_prey <- time_radians[high_dist$Species_Name == 'Lepus townsendii']

overlapPlot(high_pred, high_prey)
legend('topright', c("Predator", "Prey"), lty=c(1,2), col=c(1,4), bty='n')

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
# yes they're normal!:)

# Coyote: higher crepuscular peaks with higher human presence
# Whitetailed Jackrabbit: higher evening peak with low human presence; higher morning peak with higher human presence. No duirnal activity regardless
# Overlap: not much of a difference but slightly higher overlap with higher human presence


# Print results to "results" folder ####

# overlap graph
pdf("results/overlap_graphs/coyote_whitejackrabbit_low.pdf")
overlapPlot(low_pred, low_prey)
legend('topright', c("Coyote", "Whitetailed Jackrabbit"), lty=c(1,2), col=c(1,4), bty='n')
dev.off()

pdf("results/overlap_graphs/coyote_whitejackrabbit_high.pdf")
overlapPlot(high_pred, high_prey)
legend('topright', c("Coyote", "Whitetailed Jackrabbit"), lty=c(1,2), col=c(1,4), bty='n')
dev.off()

# results of t test
# data:  bootstrap_low and bootstrap_high
# t = -7.1939, df = 398, p-value = 3.149e-12
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#   -0.02374816 -0.01355426
# sample estimates:
#   mean of x mean of y 
# 0.7422815 0.7609327

# cutoff at median
# data:  bootstrap_low and bootstrap_high
# t = -12.591, df = 398, p-value < 2.2e-16
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#   -0.03365377 -0.02456371
# sample estimates:
#   mean of x mean of y 
# 0.7446887 0.7737974 
