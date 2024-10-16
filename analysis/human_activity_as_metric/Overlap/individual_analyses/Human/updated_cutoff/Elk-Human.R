# Elk and Human Overlap Analysis
# Margaret Mercer
# May 9, 2024

library(tidyverse)
library(overlap)
library(raster)
library(sf)
library(terra)

data <- read_csv("../data_too_big/all_years.csv")

# subset to species range only ####
# get range data
range <- st_read('data/subset_shape_files/Elk')
st_is_valid(range, reason=TRUE)

# switch off spherical geometry
sf_use_s2(FALSE)

# Convert df to an sf object
data <- read_csv("../data_too_big/all_years.csv")
points_sf <- st_make_valid(st_as_sf(data, coords = c("Longitude", "Latitude"), crs = st_crs(range)))

sf_use_s2(TRUE)

# Perform the point-in-polygon test
inside <- st_within(points_sf, range, sparse = FALSE)

# Extract rows from df that are inside the polygon
df_inside <- data[which(inside[,1]),]

# df_inside now contains only the rows where coordinates fall inside the polygon

# now make df_inside spatial for visualization
spatial_inside <- st_make_valid(st_as_sf(df_inside, coords = c("Longitude", "Latitude"), crs = st_crs(range)))

# double check that this works by visualizing on map
ggplot() +
  geom_sf(data = range[1,], size = 1.5, color = "black", fill = "#690375") +
  ggtitle("Species Range") +
  geom_sf(data = spatial_inside) +
  coord_sf()


# Overlap Analysis ####
species <- filter(df_inside, Species_Name == 'Cervus canadensis')

# find median and assign to an object
median <- median(species$Humans_Per_Camera_Per_Day)

# filter TOTAL dataframe to low human disturbance
low_dist <- filter(df_inside, Humans_Per_Camera_Per_Day < median)

# convert time into radians
time <- as.POSIXct(low_dist$Local_Time, format = "%H:%M:%S")

# Extract hours, minutes, and seconds
hours <- as.numeric(format(time, "%H"))
minutes <- as.numeric(format(time, "%M"))
seconds <- as.numeric(format(time, "%S"))

# Convert time to radians
time_radians <- 2 * pi * ((hours + minutes / 60 + seconds / 3600) / 24)

# plot for low disturbance
low_animal <- time_radians[low_dist$Species_Name == 'Cervus canadensis']
low_human <- time_radians[low_dist$Species_Name == "Homo sapiens" | low_dist$Species_Name == "Vehicle"]

pdf("results/overlap_graphs/elk_human_low.pdf")
overlapPlot(low_animal, low_human)
legend('topright', c("Elk", "Human"), lty=c(1,2), col=c(1,4), bty='n')
dev.off()

overlap_low <- overlapEst(low_animal, low_human, type="Dhat4")
overlap_low

# calculate bootstrap values
bootstrap_low <- bootstrap(low_animal, low_human, 200, smooth=TRUE, kmax=3, adjust=NA, n.grid=128, type="Dhat4", cores=1)
# now calculate confidence intervals
bootCI(overlap_low, bootstrap_low, conf = 0.95)

## plot overlap for HIGH disturbance
high_dist <- filter(df_inside, Humans_Per_Camera_Per_Day >= median)

# convert time into radians
time <- as.POSIXct(high_dist$Local_Time, format = "%H:%M:%S")

# Extract hours, minutes, and seconds
hours <- as.numeric(format(time, "%H"))
minutes <- as.numeric(format(time, "%M"))
seconds <- as.numeric(format(time, "%S"))

# Convert time to radians
time_radians <- 2 * pi * ((hours + minutes / 60 + seconds / 3600) / 24)

# plot for high disturbance
high_animal <- time_radians[high_dist$Species_Name == 'Cervus canadensis']
high_human <- time_radians[high_dist$Species_Name == "Homo sapiens" | high_dist$Species_Name == "Vehicle"]

pdf("results/overlap_graphs/elk_human_high.pdf")
overlapPlot(high_animal, high_human)
legend('topright', c("Elk", "Human"), lty=c(1,2), col=c(1,4), bty='n')
dev.off()

overlap_high <- overlapEst(high_human, high_animal, type="Dhat4")
overlap_high

# calculate bootstrap values
bootstrap_high <- bootstrap(high_human, high_animal, 200, smooth=TRUE, kmax=3, adjust=NA, n.grid=128, type="Dhat4", cores=1)
# now calculate confidence intervals
bootCI(overlap_high, bootstrap_high, conf = 0.95)


t.test(bootstrap_low, bootstrap_high, var.equal = TRUE)

# plot histogram of bootstrap values?
hist(bootstrap_low)
hist(bootstrap_high)


# Elk: higher diurnality with higher human presence
# Human: wider spread with higher human activity
# Overlap: increases a LOT with high human presence!

# t test results
# data:  bootstrap_low and bootstrap_high
# t = -253.49, df = 398, p-value < 2.2e-16
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#   -0.2834895 -0.2791261
# sample estimates:
#   mean of x mean of y 
# 0.3496060 0.6309138 

# t test results cutoff at species median
# data:  bootstrap_low and bootstrap_high
# t = -156.45, df = 398, p-value < 2.2e-16
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#   -0.2563352 -0.2499730
# sample estimates:
#   mean of x mean of y 
# 0.3534874 0.6066415
