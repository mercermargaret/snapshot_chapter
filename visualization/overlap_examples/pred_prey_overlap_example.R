# pred prey overlap example
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
high_pred <- time_radians[high_dist$Species_Name == 'Puma concolor']
high_prey <- time_radians[high_dist$Species_Name == 'Odocoileus virginianus']


# overlap examples ####
# low

# just pred
overlapPlot(low_pred, low_prey, linet = c(1,5), linec = c("red", NA), linewidth = c(2, NA), olapcol = NA,
            rug=FALSE, main="Low Human Disturbance")
legend("topleft", c("Puma"), lty = c(1,5), col=c("red", "NA"), lwd = c(2, 2), bg="white")

# add line
abline(v=c(5.5, 18+47/60), lty=3)
legend("topleft", c("Puma"), lty = c(1,5), col=c("red", "NA"), lwd = c(2, 2), bg="white")

# both pred and prey
overlapPlot(low_pred, low_prey, linet = c(1,5), linec = c("red", "blue"), linewidth = c(2, 2), olapcol = NA,
            rug=FALSE, main="Low Human Disturbance")
abline(v=c(5.5, 18+47/60), lty=3)
legend("topleft", c("Puma", "Deer"), lty = c(1,5), col=c("red", "blue"), lwd = c(2, 2), bg="white")

# add overlap shading
overlapPlot(low_pred, low_prey, linet = c(1,5), linec = c("red", "blue"), linewidth = c(2, 2),
            rug=FALSE, main="Low Human Disturbance")
abline(v=c(5.5, 18+47/60), lty=3)
legend("topleft", c("Puma", "Deer"), lty = c(1,5), col=c("red", "blue"), lwd = c(2, 2), bg="white")


# high
# just pred
overlapPlot(high_pred, high_prey, linet = c(1,5), linec = c("red", NA), linewidth = c(2, NA), olapcol = NA,
            rug=FALSE, main="High Human Disturbance")
legend("topleft", c("Puma"), lty = c(1,5), col=c("red", "NA"), lwd = c(2, 2), bg="white")

# add line
abline(v=c(5.5, 18+47/60), lty=3)
legend("topleft", c("Puma"), lty = c(1,5), col=c("red", "NA"), lwd = c(2, 2), bg="white")

# both pred and prey
overlapPlot(high_pred, high_prey, linet = c(1,5), linec = c("red", "blue"), linewidth = c(2, 2), olapcol = NA,
            rug=FALSE, main="High Human Disturbance")
abline(v=c(5.5, 18+47/60), lty=3)
legend("topleft", c("Puma", "Deer"), lty = c(1,5), col=c("red", "blue"), lwd = c(2, 2), bg="white")

# add overlap shading
overlapPlot(high_pred, high_prey, linet = c(1,5), linec = c("red", "blue"), linewidth = c(2, 2),
            rug=FALSE, main="High Human Disturbance")
abline(v=c(5.5, 18+47/60), lty=3)
legend("topleft", c("Puma", "Deer"), lty = c(1,5), col=c("red", "blue"), lwd = c(2, 2), bg="white")

