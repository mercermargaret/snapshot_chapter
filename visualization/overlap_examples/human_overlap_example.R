# human species overlap example
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

# get median
# subset to the two species
species <- filter(data, Species_Name == 'Cervus canadensis') 

# select median of sites and assign to object
sites <- species %>% 
  group_by(Site_Name) %>% 
  summarize(Humans_Per_Camera_Per_Day) %>% 
  unique()

median <- median(sites$Humans_Per_Camera_Per_Day)

## plot pred and prey overlap for LOW disturbance

low_dist <- filter(data, Humans_Per_Camera_Per_Day < median)

# convert time into radians
time <- as.POSIXct(low_dist$Local_Time, format = "%H:%M:%S")

# Extract hours, minutes, and seconds
hours <- as.numeric(format(time, "%H"))
minutes <- as.numeric(format(time, "%M"))
seconds <- as.numeric(format(time, "%S"))

# Convert time to radians
time_radians <- 2 * pi * ((hours + minutes / 60 + seconds / 3600) / 24)

# plot pred and prey for low disturbance
low_hum <- time_radians[low_dist$Species_Name == 'Homo sapiens']
low_species <- time_radians[low_dist$Species_Name == 'Cervus canadensis']

## plot pred and prey overlap for HIGH disturbance

high_dist <- filter(data, Humans_Per_Camera_Per_Day >= median)

# convert time into radians
time <- as.POSIXct(high_dist$Local_Time, format = "%H:%M:%S")

# Extract hours, minutes, and seconds
hours <- as.numeric(format(time, "%H"))
minutes <- as.numeric(format(time, "%M"))
seconds <- as.numeric(format(time, "%S"))

# Convert time to radians
time_radians <- 2 * pi * ((hours + minutes / 60 + seconds / 3600) / 24)

# plot pred and prey for high disturbance
high_hum <- time_radians[high_dist$Species_Name == 'Homo sapiens']
high_species <- time_radians[high_dist$Species_Name == 'Cervus canadensis']


# overlap examples ####
# low

# empty
overlapPlot(low_hum, low_species, linet = c(1,5), linec = c(NA, NA), linewidth = c(2, NA), olapcol = NA,
            rug=FALSE, main="Low Human Activity", ylab = NA, yaxt = "n")
abline(v=c(5.5, 18+47/60), lty=3)

# just human
overlapPlot(low_hum, low_species, linet = c(1,5), linec = c("red", NA), linewidth = c(2, NA), olapcol = NA,
            rug=FALSE, main="Low Human Activity", ylab = NA, yaxt = "n")
abline(v=c(5.5, 18+47/60), lty=3)
legend("topleft", c("Human"), lty = c(1,5), col=c("red", "NA"), lwd = c(2, 2), bg="white")

# both human and animal
overlapPlot(low_hum, low_species, linet = c(1,5), linec = c("red", "blue"), linewidth = c(2, 2), olapcol = NA,
            rug=FALSE, main="Low Human Activity", ylab = NA, yaxt = "n")
abline(v=c(5.5, 18+47/60), lty=3)
legend("topleft", c("Human", "Elk"), lty = c(1,5), col=c("red", "blue"), lwd = c(2, 2), bg="white")

# add overlap shading
overlapPlot(low_hum, low_species, linet = c(1,5), linec = c("red", "blue"), linewidth = c(2, 2),
            rug=FALSE, main="Low Human Activity", ylab = NA, yaxt = "n")
abline(v=c(5.5, 18+47/60), lty=3)
legend("topleft", c("Human", "Elk"), lty = c(1,5), col=c("red", "blue"), lwd = c(2, 2), bg="white")


# high
# empty
overlapPlot(high_hum, high_species, linet = c(1,5), linec = c(NA, NA), linewidth = c(2, NA), olapcol = NA,
            rug=FALSE, main="High Human Activity", ylab = NA, yaxt = "n")
# add line
abline(v=c(5.5, 18+47/60), lty=3)

# just human
overlapPlot(high_hum, high_species, linet = c(1,5), linec = c("red", NA), linewidth = c(2, NA), olapcol = NA,
            rug=FALSE, main="High Human Activity", ylab = NA, yaxt = "n")
# add line
abline(v=c(5.5, 18+47/60), lty=3)
legend("topleft", c("Human"), lty = c(1,5), col=c("red", "NA"), lwd = c(2, 2), bg="white")

# both human and animal
overlapPlot(high_hum, high_species, linet = c(1,5), linec = c("red", "blue"), linewidth = c(2, 2), olapcol = NA,
            rug=FALSE, main="High Human Activity", ylab = NA, yaxt = "n")
abline(v=c(5.5, 18+47/60), lty=3)
legend("topleft", c("Human", "Elk"), lty = c(1,5), col=c("red", "blue"), lwd = c(2, 2), bg="white")

# add overlap shading
overlapPlot(high_hum, high_species, linet = c(1,5), linec = c("red", "blue"), linewidth = c(2, 2),
            rug=FALSE, main="High Human Activity", ylab = NA, yaxt = "n")
abline(v=c(5.5, 18+47/60), lty=3)
legend("topleft", c("Human", "Elk"), lty = c(1,5), col=c("red", "blue"), lwd = c(2, 2), bg="white")

