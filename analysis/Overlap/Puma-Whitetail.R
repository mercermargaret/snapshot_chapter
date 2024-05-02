# Puma and Whitetail Overlap Analysis
# Margaret Mercer
# April 16, 2024


library(tidyverse)
library(overlap)
library(raster)
library(sf)
library(terra)

# Range Subsetting ####

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
low_prey <- time_radians[low_dist$Species_Name == 'Odocoileus virginianus']

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
high_prey <- time_radians[high_dist$Species_Name == 'Odocoileus virginianus']

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

# Puma mostly consistently active with low human disturbance, but VERY nocturnal with high human disturbance. 
# Whitetails remain crepuscular throughout BUT dip in middle is stronger in high human areas! SO they're ALSO becoming more nocturnal!
# Greater overlap between puma and whitetail in areas of LOW human presence than in areas of HIGH human presence (p value = 0.07)


# Print results to "results" folder ####

# overlap graph
pdf("results/overlap_graphs/puma_whitetail_low.pdf")
overlapPlot(low_pred, low_prey)
legend('topright', c("Puma", "Whitetail"), lty=c(1,2), col=c(1,4), bty='n')
dev.off()

pdf("results/overlap_graphs/puma_whitetail_high.pdf")
overlapPlot(high_pred, high_prey)
legend('topright', c("Puma", "Whitetail"), lty=c(1,2), col=c(1,4), bty='n')
dev.off()

# results of t test

# FIRST t test 
# data:  bootstrap_low and bootstrap_high
# t = 2.6527, df = 398, p-value = 0.008305
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#   0.002285536 0.015371395
# sample estimates:
#   mean of x mean of y 
# 0.8544174 0.8455890 

# subsequent t tests:
# data:  bootstrap_low and bootstrap_high
# t = 0.89752, df = 398, p-value = 0.37
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#   -0.003841762  0.010296264
# sample estimates:
#   mean of x mean of y 
# 0.8507051 0.8474779

# data:  bootstrap_low and bootstrap_high
# t = 2.0164, df = 398, p-value = 0.04443
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#   0.0001717711 0.0135617553
# sample estimates:
#   mean of x mean of y 
# 0.8503962 0.8435295

# data:  bootstrap_low and bootstrap_high
# t = 2.4541, df = 398, p-value = 0.01455
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#   0.001628651 0.014746061
# sample estimates:
#   mean of x mean of y 
# 0.8501632 0.8419758 

