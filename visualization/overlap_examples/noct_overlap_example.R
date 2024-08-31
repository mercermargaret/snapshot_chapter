# example of change in nocturnality from low to high human presence
# margaret mercer
# august 22, 2024

# packages
library(dplyr)
library(tidyr)
library(overlap)

# data
data <- read.csv("../data_too_big/five_year_observation_data.csv")

# split local_date_time into date and time
data$Local_Date_Time <- as.character(data$Local_Date_Time)
data <- separate(data, Local_Date_Time, into = c("Local_Date", "Local_Time"), sep = " ")

species <- filter(data, Species_Name == 'Canis latrans') 

sites <- species %>% 
  group_by(Site_Name) %>% 
  summarize(Humans_Per_Camera_Per_Day) %>% 
  unique()

median <- median(sites$Humans_Per_Camera_Per_Day)

# low disturbance

low_dist <- filter(data, Humans_Per_Camera_Per_Day < median)

# convert time into radians
time <- as.POSIXct(low_dist$Local_Time, format = "%H:%M:%S")

# Extract hours, minutes, and seconds
hours <- as.numeric(format(time, "%H"))
minutes <- as.numeric(format(time, "%M"))
seconds <- as.numeric(format(time, "%S"))

# Convert time to radians
time_radians <- 2 * pi * ((hours + minutes / 60 + seconds / 3600) / 24)

low_species <- time_radians[low_dist$Species_Name == 'Canis latrans']

# high disturbance

high_dist <- filter(data, Humans_Per_Camera_Per_Day >= median)

# convert time into radians
time <- as.POSIXct(high_dist$Local_Time, format = "%H:%M:%S")

# Extract hours, minutes, and seconds
hours <- as.numeric(format(time, "%H"))
minutes <- as.numeric(format(time, "%M"))
seconds <- as.numeric(format(time, "%S"))

# Convert time to radians
time_radians <- 2 * pi * ((hours + minutes / 60 + seconds / 3600) / 24)

high_species <- time_radians[high_dist$Species_Name == 'Canis latrans']

# noct example ####

# empty
overlapPlot(low_species, high_species, linet = c(1,5), linec = c(NA, NA), linewidth = c(2, 2), olapcol = NA,
            rug=FALSE, main="Coyote Activity in Wild Vs Urban Areas", ylab = NA, yaxt = "n")
# add line
abline(v=c(5.5, 18+47/60), lty=3)


# just low
overlapPlot(low_species, high_species, linet = c(1,5), linec = c("darkgreen", NA), linewidth = c(2, 2), olapcol = NA,
            rug=FALSE, main="Coyote Activity in Wild Vs Urban Areas", ylab = NA, yaxt = "n")
abline(v=c(5.5, 18+47/60), lty=3)
legend("bottomleft", c("Wild Areas"), lty=c(1), lwd = c(2, 2), col=c("darkgreen"), bg="white")


# both
overlapPlot(low_species, high_species, linet = c(1,5), linec = c("darkgreen", "purple"), linewidth = c(2, 2), olapcol = NA,
            rug=FALSE, main="Coyote Activity in Wild Vs Urban Areas", ylab = NA, yaxt = "n")
abline(v=c(5.5, 18+47/60), lty=3)
legend("bottomleft", c("Wild Areas", "Urban Areas"), lty=c(1,5), lwd = c(2, 2), col=c("darkgreen", "purple"), bg="white")

