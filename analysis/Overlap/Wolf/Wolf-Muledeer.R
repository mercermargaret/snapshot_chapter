# Wolf and Mule Deer Overlap Analysis
# Margaret Mercer
# April 16, 2024


library(tidyverse)
library(overlap)
library(raster)
library(sf)
library(terra)

# Range Subsetting ####

# get range data
prey_range <- st_read('data/subset_shape_files/Mule Deer')
pred_range <- st_read('data/subset_shape_files/Wolf')
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

# df_inside now contains only the rows of camera trap data where coordinates fall inside the polygon

# now make df_inside spatial
spatial_inside <- st_make_valid(st_as_sf(df_inside, coords = c("Longitude", "Latitude"), crs = st_crs(prey_range)))

# double check that this works by visualizing on map
ggplot() +
  geom_sf(data = prey_range, size = 1.5, color = "black", fill = "#0075C4", alpha = 0.5) +
  geom_sf(data = pred_range, size = 1.5, color = "black", fill = "#CB429F", alpha = 0.5) +
  geom_sf(data = range_overlap, size = 1.5, color = "black", fill = "#690375") +
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
low_pred <- time_radians[low_dist$Species_Name == 'Canis lupus']
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
high_pred <- time_radians[high_dist$Species_Name == 'Canis lupus']
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

# plot histogram of bootstrap values?
hist(bootstrap_low)
hist(bootstrap_high)
# yes they're normal!:)

# Wolf: shift to later in the day with higher human presence
# Mule deer: with high human disturbance, a bit more diurnal activity and a MUCH higher peak in the evening than in morning (whereas with low, peaks are same height)
# Overlap in high vs low: overlap greater in higher human disturbance areas than in lower, but BARELY (0.64 vs 0.65, and p ~ 0.05 - 0.5)


# Print results to "results" folder ####

# overlap graph
pdf("results/overlap_graphs/wolf_muledeer_low.pdf")
overlapPlot(low_pred, low_prey)
legend('topright', c("Wolf", "Mule Deer"), lty=c(1,2), col=c(1,4), bty='n')
dev.off()

pdf("results/overlap_graphs/wolf_muledeer_high.pdf")
overlapPlot(high_pred, high_prey)
legend('topright', c("Wolf", "Mule Deer"), lty=c(1,2), col=c(1,4), bty='n')
dev.off()

# results of t test

# first round before changing spatial stuff
# data:  bootstrap_low and bootstrap_high
# t = -0.77468, df = 398, p-value = 0.439
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#   -0.013699472  0.005954711
# sample estimates:
#   mean of x mean of y 
# 0.6405975 0.6444699

# second round
# data:  bootstrap_low and bootstrap_high
# t = -2.154, df = 398, p-value = 0.03184
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#   -0.0211215886 -0.0009643109
# sample estimates:
#   mean of x mean of y 
# 0.6454627 0.6565057 

# data:  bootstrap_low and bootstrap_high
# t = -1.8517, df = 398, p-value = 0.06481
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#   -0.0187275807  0.0005603049
# sample estimates:
#   mean of x mean of y 
# 0.6422594 0.6513430
