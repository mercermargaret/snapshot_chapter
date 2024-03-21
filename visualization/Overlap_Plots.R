# Overlap Plots
# Margaret Mercer
# February 7, 2024

install.packages("overlap")
# https://cran.r-project.org/web/packages/overlap/overlap.pdf
# https://rdrr.io/cran/overlap/f/inst/doc/overlap.pdf

library(dplyr)
library(tidyr)
library(overlap)

data <- read.csv("all_years.csv")

data <- separate(data, Local_Date_Time, c("Date", "Time"), sep = " ")

## Convert time into radians
time <- as.POSIXct(data$Time, format = "%H:%M:%S")
# Extract hours, minutes, and seconds
hours <- as.numeric(format(time, "%H"))
minutes <- as.numeric(format(time, "%M"))
seconds <- as.numeric(format(time, "%S"))
# Convert time to radians
time_radians <- 2 * pi * ((hours + minutes / 60 + seconds / 3600) / 24)

## plot stuff
# density plot for humans
time_radians[data$Species_Name == 'Homo sapiens'] %>% 
  densityPlot(rug=TRUE, adjust = 1)

# density plot for predators
time_radians[data$Species_Name == 'Puma concolor'] %>% 
  densityPlot(rug=TRUE, adjust = 1)

# density plot for prey
time_radians[data$Species_Name == 'Odocoileus virginianus'] %>% 
  densityPlot(rug=TRUE, adjust = 1)

# plot pred and prey
pred <- time_radians[data$Species_Name == 'Puma concolor']
human <- time_radians[data$Species_Name == 'Homo sapiens']

overlapPlot(pred, human)
legend('topright', c("Predator", "Human"), lty=c(1,2), col=c(1,4), bty='n')

# visualize data to help determine what values to designate as "high" and "low" human activity
# pred <- subset(data, Species_Name == "Puma concolor")
# hist(pred$Humans_Per_Camera_Per_Day)
# hist(log(pred$Humans_Per_Camera_Per_Day))
# median(pred$Humans_Per_Camera_Per_Day)
# 
# prey <- subset(data, Species_Name == "Odocoileus virginianus")
# hist(prey$Humans_Per_Camera_Per_Day)
# hist(log(prey$Humans_Per_Camera_Per_Day))
# median(prey$Humans_Per_Camera_Per_Day)

# avg median of the two is 0.1, so let's use that as cutoff for "high" vs "low" dist

## plot pred and prey overlap for LOW disturbance

low_dist <- filter(data, Humans_Per_Camera_Per_Day < 0.1)

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
low_prey <- time_radians[low_dist$Species_Name == 'Odocoileus virginianus']

overlapPlot(low_pred, low_prey)
legend('topright', c("Predator", "Prey"), lty=c(1,2), col=c(1,4), bty='n')




## plot pred and prey overlap for HIGH disturbance

high_dist <- filter(data, Humans_Per_Camera_Per_Day > 0.1)

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
high_prey <- time_radians[high_dist$Species_Name == 'Odocoileus virginianus']

overlapPlot(high_pred, high_prey)
legend('topright', c("Predator", "Prey"), lty=c(1,2), col=c(1,4), bty='n')


# prey in high human activity vs prey in low human activity
overlapPlot(high_prey, low_prey)
legend('topright', c("High Human Activity", "Low Human Activity"), lty=c(1,2), col=c(1,4), bty='n')



# Plot examples from package (with different options and formatting)
overlapPlot(pred, human)
# Make it prettier:
overlapPlot(pred, human, linet = c(1,1), linec = c("red", "blue"),
            rug=TRUE, extend="lightgreen", main="Simulated data")
legend("topleft", c("Puma", "Human"), lty=1, col=c("red", "blue"), bg="white")
# Add vertical dotted lines to mark sunrise (05:30) and sunset (18:47):
# (times must be in hours if the x-axis is labelled in hours)
abline(v=c(5.5, 18+47/60), lty=3)
# A plot centered on midnight:
overlapPlot(pred, human, xcenter = "m", rug=TRUE)
# Mark sunrise/sunset; values to the left of "00:00" are negative
# so subtract 24:
abline(v=c(5.5, (18+47/60) - 24), lty=3)
