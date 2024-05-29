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
prey_range <- st_read('data/subset_shape_files/Mule Deer')
pred_range <- st_read('data/subset_shape_files/Puma')
st_is_valid(prey_range, reason=TRUE)
st_is_valid(pred_range, reason=TRUE)

# switch off spherical geometry
sf_use_s2(FALSE)

# now calculate overlap
range_overlap <- st_intersection(pred_range, prey_range)
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
# ggplot() +
#   geom_sf(data = prey_range, size = 1.5, color = "black", fill = "#0075C4", alpha = 0.5) +
#   geom_sf(data = pred_range, size = 1.5, color = "black", fill = "#CB429F", alpha = 0.5) +
#   geom_sf(data = range_overlap[1,], size = 1.5, color = "black", fill = "#690375") +
#   ggtitle("Predator/Prey Overlap") +
#   geom_sf(data = spatial_inside) +
#   coord_sf()


# Overlap Analysis ####

# subset to the two species

pair <- filter(df_inside, Species_Name == 'Puma concolor' | Species_Name == 'Odocoileus hemionus') 

# find median and assign to an object

median <- median(pair$Humans_Per_Camera_Per_Day)


# filter to low human disturbance
low_dist <- filter(pair, Humans_Per_Camera_Per_Day < median)

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

high_dist <- filter(pair, Humans_Per_Camera_Per_Day >= median)

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

# plot histogram of bootstrap values?
hist(bootstrap_low)
hist(bootstrap_high)
# yes they're normal!:)

# Puma: strong shift to nocturnality with high human presence
# Mule deer: strong crepuscular peaks flatten with higher human disturbance; noct increases slightly
# Overlap: greater overlap in areas of low human disturbance than high (p < 0.01)


# Print results to "results" folder ####

# overlap graph
pdf("results/overlap_graphs/puma_muledeer_low.pdf")
overlapPlot(low_pred, low_prey)
legend('topright', c("Puma", "Mule Deer"), lty=c(1,2), col=c(1,4), bty='n')
dev.off()

pdf("results/overlap_graphs/puma_muledeer_high.pdf")
overlapPlot(high_pred, high_prey)
legend('topright', c("Puma", "Mule Deer"), lty=c(1,2), col=c(1,4), bty='n')
dev.off()

# results of t test

# before updating spatial stuff:
# data:  bootstrap_low and bootstrap_high
# t = 25.487, df = 398, p-value < 2.2e-16
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#   0.07091575 0.08277013
# sample estimates:
#   mean of x mean of y 
# 0.8415682 0.7647252 

# after updating spatial stuff:
# data:  bootstrap_low and bootstrap_high
# t = 22.505, df = 398, p-value < 2.2e-16
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#   0.06827058 0.08133986
# sample estimates:
#   mean of x mean of y 
# 0.8355367 0.7607315

# with cutoff at median
# data:  bootstrap_low and bootstrap_high
# t = 7.6513, df = 398, p-value = 1.515e-13
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#   0.01894782 0.03205167
# sample estimates:
#   mean of x mean of y 
# 0.8155387 0.7900390 

# with cutoff at median of pair
# data:  bootstrap_low and bootstrap_high
# t = 8.1335, df = 398, p-value = 5.364e-15
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#   0.02194147 0.03592940
# sample estimates:
#   mean of x mean of y 
# 0.8166270 0.7876916
