# visualize shift in nocturnality within a single species
# Margaret Mercer
# June 20, 2024

library(overlap)
library(tidyverse)

# Overlap Analysis ####
data <- read_csv("../data_too_big/all_years.csv")

# subset to the species
species <- filter(data, Species_Name == 'Canis latrans')

# find median and assign to an object
median <- median(species$Humans_Per_Camera_Per_Day)

# filter to low human disturbance
low_dist <- filter(species, Humans_Per_Camera_Per_Day < median)

# convert time into radians
time <- as.POSIXct(low_dist$Local_Time, format = "%H:%M:%S")

# Extract hours, minutes, and seconds
hours <- as.numeric(format(time, "%H"))
minutes <- as.numeric(format(time, "%M"))
seconds <- as.numeric(format(time, "%S"))

# Convert time to radians
time_radians <- 2 * pi * ((hours + minutes / 60 + seconds / 3600) / 24)

# plot pred and prey for low disturbance
low_dist <- time_radians[low_dist$Species_Name == 'Canis latrans']

# now high_dist
high_dist <- filter(species, Humans_Per_Camera_Per_Day >= median)
# convert time into radians
time <- as.POSIXct(high_dist$Local_Time, format = "%H:%M:%S")

# Extract hours, minutes, and seconds
hours <- as.numeric(format(time, "%H"))
minutes <- as.numeric(format(time, "%M"))
seconds <- as.numeric(format(time, "%S"))

# Convert time to radians
time_radians <- 2 * pi * ((hours + minutes / 60 + seconds / 3600) / 24)

# plot pred and prey for high disturbance
high_dist <- time_radians[high_dist$Species_Name == 'Canis latrans']

overlapPlot(low_dist, high_dist)
legend('topright', c("Low Dist", "High Dist"), lty=c(1,2), col=c(1,4), bty='n')

