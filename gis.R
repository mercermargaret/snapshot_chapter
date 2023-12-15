# GIS TUTORIAL ####
# Jesse Alston 
# 21 October 2023

# PACKAGES ####
library(terra)
library(ggplot2)
# library(gridExtra)

# DATA ####
# Read Data
tapir_df <- read.csv("FILEPATH")
tc_r <- rast("FILEPATH")

tapir1 <- tapir_df[tapir_df$individual.local.identifier=="PA_20_SY",]

# Crop raster
e <- extent(min(tapir1$location.long) - 0.05, max(tapir1$location.long) + 0.05, 
            min(tapir1$location.lat) - 0.05, max(tapir1$location.lat) + 0.05)
tc_rc <- terra::crop(tc_r, e)

# Check raster
terra::plot(tc_rc)
points(tapir1$location.lat ~ tapir1$location.long)

# Extract values for tapir locations
xy <- as.matrix(tapir1[,4:5])
tapir1[,7] <- terra::extract(tc_rc, xy)

# Randomly sample 1322 points for comparison
rand_y <- runif(1322, min = ext(tc_rc)[3], max = ext(tc_rc)[4])
rand_x <- runif(1322, min = ext(tc_rc)[1], max = ext(tc_rc)[2])
uid <- seq(1, 1322, 1)
rand_df <- as.data.frame(cbind(uid, rand_x, rand_y))

# Check raster
terra::plot(tc_rc)
points(rand_df$rand_y ~ rand_df$rand_x)
points(tapir1$location.lat ~ tapir1$location.long, col="purple")

rand_xy <- as.matrix(rand_df[,2:3])
rand_df[,4] <- terra::extract(tc_rc, rand_xy)

# Create histogram
theme_set(theme_classic())

p <- ggplot() + 
  geom_histogram(data=rand_df, aes(x=treecover2010), color="darkblue", fill="darkblue", alpha=0.5, bins=10) +
  geom_histogram(data=tapir1, aes(x=treecover2010), color="lightblue", fill="lightblue", alpha=0.5, bins=10)
p

# grid.arrange(p1, p2, p3, p4, ncol=2)


# Exercise: Download an animal movement data set from MoveBank, download the "ml-hfi" raster on D2L,
# and repeat the analysis above with 4 individual animals. Submit your histograms to D2L.