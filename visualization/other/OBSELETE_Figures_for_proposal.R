### OBSELETE

# Create Figures For Proposal
# Margaret Mercer
# February 14, 2024

# install.packages("overlap")
# https://cran.r-project.org/web/packages/overlap/overlap.pdf
# https://rdrr.io/cran/overlap/f/inst/doc/overlap.pdf

library(dplyr)
library(overlap)

data <- read.csv("../data_too_big/five_year_observation_data.csv")

# split local_date_time into date and time


# get median
# subset to the two species
pair <- filter(data, Species_Name == 'Puma concolor' | Species_Name == 'Odocoileus hemionus') 

# select median of sites and assign to object
sites <- pair %>% 
  group_by(Site_Name) %>% 
  summarize(Humans_Per_Camera_Per_Day) %>% 
  unique()

median <- median(sites$Humans_Per_Camera_Per_Day)

## plot pred and prey overlap for LOW disturbance

low_dist <- filter(data, Humans_Per_Camera_Per_Day < median)

# convert time into radians
time <- as.POSIXct(low_dist$Time, format = "%H:%M:%S")

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

## plot pred and prey overlap for HIGH disturbance

high_dist <- filter(data, Humans_Per_Camera_Per_Day > median)

# convert time into radians
time <- as.POSIXct(high_dist$Time, format = "%H:%M:%S")

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
