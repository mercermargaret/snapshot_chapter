# Overlap Package Shenanigans
# Margaret Mercer
# February 7, 2024

install.packages("overlap")
# https://cran.r-project.org/web/packages/overlap/overlap.pdf
# https://rdrr.io/cran/overlap/f/inst/doc/overlap.pdf

library(dplyr)
library(overlap)

data <- read.csv("all_years.csv")

## convert times to radians

datetime <- as.POSIXct(data$Date_Time, format = "%Y-%m-%d %H:%M:%S")
sum(is.na(datetime))
## RIGHT here, 661 values are getting turned into NAs
which(is.na(datetime))
# --> they don't have times, just dates. BUT the altitudes are there. Why??
# whatever. for now we'll just ignore the NAs.
# PROBLEM: how do I get rid of NAs in this situation?

hour <- as.numeric(format(datetime, "%H"))
minute <- as.numeric(format(datetime, "%M"))
time_radians <- 2 * pi * ((hour + minute / 60) / 24)

# density plot for humans
time_radians[data$Species_Name == 'Homo sapiens'] %>% 
  densityPlot(rug=TRUE)

# density plot for predators
time_radians[data$Species_Name == 'Puma concolor'] %>% 
  densityPlot(rug=TRUE)

# density plot for prey
time_radians[data$Species_Name == 'Odocoileus hemionus'] %>% 
  densityPlot(rug=TRUE)

# plot pred and prey
pred <- time_radians[data$Species_Name == 'Puma concolor']
prey <- time_radians[data$Species_Name == 'Odocoileus hemionus']

overlapPlot(pred, prey)
legend('topright', c("Predator", "Prey"), lty=c(1,2), col=c(1,4), bty='n')

## Code humans/trapping day/camera
# create new column with 1 for human or vehicle
data$Human <- ifelse(data$Species_Name == "Homo sapiens" | data$Species_Name == "Vehicle", 1, 0)
Humans_Per_Camera <- data %>% filter(Human == 1) %>%
  group_by(Site_Name) %>%
  summarise(Humans_Per_Camera = n())
data <- merge(data, Humans_Per_Camera, by = "Site_Name")
data$Humans_Per_Camera_Per_Day <- data$Humans_Per_Camera/data$Survey_Days

# visualize data to help determine what values to designate as "high" and "low" human activity
# pred <- subset(data, Species_Name == "Puma concolor")
# hist(pred$Humans_Per_Camera_Per_Day)
# hist(log(pred$Humans_Per_Camera_Per_Day))
# median(pred$Humans_Per_Camera_Per_Day)
# 
# prey <- subset(data, Species_Name == "Odocoileus hemionus")
# hist(prey$Humans_Per_Camera_Per_Day)
# hist(log(prey$Humans_Per_Camera_Per_Day))
# median(prey$Humans_Per_Camera_Per_Day)
#
# avg median of the two is 0.145, so let's use that as cutoff for "high" vs "low" dist

## plot pred and prey overlap for LOW disturbance

low_dist <- filter(data, Humans_Per_Camera_Per_Day < 0.145)

# convert time into radians
datetime <- as.POSIXct(low_dist$Date_Time, format = "%Y-%m-%d %H:%M:%S")
hour <- as.numeric(format(datetime, "%H"))  # Extract hour (24-hour format)
minute <- as.numeric(format(datetime, "%M"))  # Extract minute
time_radians <- 2 * pi * ((hour + minute / 60) / 24)

# plot pred and prey for low disturbance
low_pred <- time_radians[low_dist$Species_Name == 'Puma concolor']
low_prey <- time_radians[low_dist$Species_Name == 'Odocoileus hemionus']

overlapPlot(low_pred, low_prey)
legend('topright', c("Predator", "Prey"), lty=c(1,2), col=c(1,4), bty='n')




## plot pred and prey overlap for HIGH disturbance

high_dist <- filter(data, Humans_Per_Camera_Per_Day > 0.145)

# get time into radians
datetime <- as.POSIXct(high_dist$Date_Time, format = "%Y-%m-%d %H:%M:%S")
hour <- as.numeric(format(datetime, "%H"))  # Extract hour (24-hour format)
minute <- as.numeric(format(datetime, "%M"))  # Extract minute
time_radians <- 2 * pi * ((hour + minute / 60) / 24)

# plot pred and prey for high disturbance
high_pred <- time_radians[high_dist$Species_Name == 'Puma concolor']
high_prey <- time_radians[high_dist$Species_Name == 'Odocoileus hemionus']

overlapPlot(high_pred, high_prey)
legend('topright', c("Predator", "Prey"), lty=c(1,2), col=c(1,4), bty='n')

# PROBLEM: how come when I split it into high and low, there's more NAs than before??
