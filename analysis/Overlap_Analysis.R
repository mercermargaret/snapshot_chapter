# Overlap Analysis
# Margaret Mercer
# March 13, 2024

# https://cran.r-project.org/web/packages/overlap/overlap.pdf

library(tidyverse)
library(overlap)


data <- read_csv("../data_too_big/all_years.csv")


# ## what if i only want to include prey in areas where predators are present?
# IDK how to do this so it works, so I'm putting a pin in it for now
# # subset
# pred <- subset(data, Species_Name == 'Puma concolor')
# prey <- subset(data, Species_Name == 'Odocoileus virginianus')
# human <- subset(data, Species_Name == 'Homo sapiens')
# # add column to prey dataframe that has predator present as a 0 or 1
# prey$Pred_Present <- as.numeric(prey$Array %in% pred$Array) 
# # subset prey into "with pred" and "without pred"
# withpred <- subset(prey, Pred_Present == 1)

## Convert time into radians
time <- as.POSIXct(data$Local_Time, format = "%H:%M:%S")
# Extract hours, minutes, and seconds
hours <- as.numeric(format(time, "%H"))
minutes <- as.numeric(format(time, "%M"))
seconds <- as.numeric(format(time, "%S"))
# Convert time to radians
time_radians <- 2 * pi * ((hours + minutes / 60 + seconds / 3600) / 24)


pred <- time_radians[data$Species_Name == 'Puma concolor']
prey <- time_radians[data$Species_Name == 'Odocoileus virginianus']
human <- time_radians[data$Species_Name == 'Homo sapiens']


# pred and prey ####
overlapPlot(pred, prey)
legend('topright', c("Puma", "Deer"), lty=c(1,2), col=c(1,4), bty='n')

# Calculate overlap density
# overlapTrue(prey, pred) # this is not accurate
overlap1 <- overlapEst(pred, prey, type="Dhat4") # "Simulations show that Dhat4 is best when the smallest sample has at least 50 observations."
overlap1

# calculate bootstrap values for that statistic
bootstrap1 <- bootstrap(pred, prey, 200, smooth=TRUE, kmax=3, adjust=NA, n.grid=128, type="Dhat4", cores=1) # I used 200 because that's what Ridout & Linkie (2009) used
# # so I think you do the above OR the below?
# resample(x, 200, smooth = TRUE, kmax = 3, adjust = 1, n.grid = 512)
# bootEst(Amat, Bmat, kmax = 3, adjust=c(0.8, 1, 4), n.grid = 128, type="Dhat4", cores=1)

# now calculate confidence intervals
bootCI(overlap1, bootstrap1, conf = 0.95)

# pred and human ####

overlapPlot(pred, human)
legend('topright', c("Puma", "Human"), lty=c(1,2), col=c(1,4), bty='n')

# Calculate overlap density
# overlapTrue(pred, human) # this is not accurate
overlap2 <- overlapEst(pred, human, type="Dhat4") # "Simulations show that Dhat4 is best when the smallest sample has at least 50 observations."
overlap2

# calculate bootstrap values
bootstrap2 <- bootstrap(pred, human, 200, smooth=TRUE, kmax=3, adjust=NA, n.grid=128, type="Dhat4", cores=1)
# now calculate confidence intervals
bootCI(overlap2, bootstrap2, conf = 0.95)

# a t test just for fun, to see how it works on these two (they should have no overlap!)
t.test(bootstrap1, bootstrap2)
# cool, so they are quite sure that the true difference between the estimates for human/pred overlap and pred/prey overlap are differnt; this checks out


# low vs high human disturbance estimates: whitetail vs puma ####
## plot pred and prey overlap for LOW disturbance

low_dist <- filter(data, Humans_Per_Camera_Per_Day < 0.1)

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

overlapPlot(low_pred, low_prey)
legend('topright', c("Predator", "Prey"), lty=c(1,2), col=c(1,4), bty='n')

overlap3 <- overlapEst(low_pred, low_prey, type="Dhat4")
overlap3

# calculate bootstrap values
bootstrap3 <- bootstrap(low_pred, low_prey, 200, smooth=TRUE, kmax=3, adjust=NA, n.grid=128, type="Dhat4", cores=1)
# now calculate confidence intervals
bootCI(overlap3, bootstrap3, conf = 0.95)

## plot pred and prey overlap for HIGH disturbance

high_dist <- filter(data, Humans_Per_Camera_Per_Day > 0.1)

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

overlapPlot(high_pred, high_prey)
legend('topright', c("Predator", "Prey"), lty=c(1,2), col=c(1,4), bty='n')

overlap4 <- overlapEst(high_pred, high_prey, type="Dhat4")
overlap4

# calculate bootstrap values
bootstrap4 <- bootstrap(high_pred, high_prey, 200, smooth=TRUE, kmax=3, adjust=NA, n.grid=128, type="Dhat4", cores=1)
# now calculate confidence intervals
bootCI(overlap4, bootstrap4, conf = 0.95)


t.test(bootstrap3, bootstrap4, var.equal = TRUE)
# ok so there's definitely a difference between the two; but is a t test the right method?

# plot histogram of bootstrap values?
hist(bootstrap3)
hist(bootstrap4)
# yes they're normal!:)

# puma mostly consistently active with low human disturbance, but VERY nocturnal with high human disturbance. 
# Prey don't shift at all from crepuscular
# greater overlap in areas of LOW human presence than in areas of HIGH human presence

# low vs high disturbance: grizzly bear vs moose ####
## plot pred and prey overlap for LOW disturbance

low_dist <- filter(data, Humans_Per_Camera_Per_Day < 0.1)

# convert time into radians
time <- as.POSIXct(low_dist$Local_Time, format = "%H:%M:%S")

# Extract hours, minutes, and seconds
hours <- as.numeric(format(time, "%H"))
minutes <- as.numeric(format(time, "%M"))
seconds <- as.numeric(format(time, "%S"))

# Convert time to radians
time_radians <- 2 * pi * ((hours + minutes / 60 + seconds / 3600) / 24)

# plot pred and prey for low disturbance
low_pred <- time_radians[low_dist$Species_Name == 'Ursus arctos']
low_prey <- time_radians[low_dist$Species_Name == 'Alces alces']

overlapPlot(low_pred, low_prey)
legend('topright', c("Predator", "Prey"), lty=c(1,2), col=c(1,4), bty='n')

overlap5 <- overlapEst(low_pred, low_prey, type="Dhat4")
overlap5

# calculate bootstrap values
bootstrap5 <- bootstrap(low_pred, low_prey, 200, smooth=TRUE, kmax=3, adjust=NA, n.grid=128, type="Dhat4", cores=1)
# now calculate confidence intervals
bootCI(overlap5, bootstrap5, conf = 0.95)

## plot pred and prey overlap for HIGH disturbance

high_dist <- filter(data, Humans_Per_Camera_Per_Day > 0.1)

# convert time into radians
time <- as.POSIXct(high_dist$Local_Time, format = "%H:%M:%S")


# Extract hours, minutes, and seconds
hours <- as.numeric(format(time, "%H"))
minutes <- as.numeric(format(time, "%M"))
seconds <- as.numeric(format(time, "%S"))

# Convert time to radians
time_radians <- 2 * pi * ((hours + minutes / 60 + seconds / 3600) / 24)

# plot pred and prey for high disturbance
high_pred <- time_radians[high_dist$Species_Name == 'Ursus arctos']
high_prey <- time_radians[high_dist$Species_Name == 'Alces alces']

overlapPlot(high_pred, high_prey)
legend('topright', c("Predator", "Prey"), lty=c(1,2), col=c(1,4), bty='n')

overlap6 <- overlapEst(high_pred, high_prey, type="Dhat4")
overlap6

# calculate bootstrap values
bootstrap6 <- bootstrap(high_pred, high_prey, 200, smooth=TRUE, kmax=3, adjust=NA, n.grid=128, type="Dhat4", cores=1)
# now calculate confidence intervals
bootCI(overlap6, bootstrap6, conf = 0.95)


# plot histogram of bootstrap values to check for normality
hist(bootstrap5)
hist(bootstrap6)


t.test(bootstrap5, bootstrap6, var.equal = TRUE)

# significant difference in overlap between the two. Moose mostly stay the same (crepuscular) between low and high, 
# but do show an increase in diurnal activity (a bit) with high human presence; 
# and grizzlies (which tend to be crepuscular) shift from consistently crepuscular, to mostly nocturnal
# higher overlap in areas of LOW human presence than HIGH human presence

# low vs high disturbance: wolf vs elk ####
## plot pred and prey overlap for LOW disturbance

low_dist <- filter(data, Humans_Per_Camera_Per_Day < 0.1)

# convert time into radians
time <- as.POSIXct(low_dist$Local_Time, format = "%H:%M:%S")

# Extract hours, minutes, and seconds
hours <- as.numeric(format(time, "%H"))
minutes <- as.numeric(format(time, "%M"))
seconds <- as.numeric(format(time, "%S"))

# Convert time to radians
time_radians <- 2 * pi * ((hours + minutes / 60 + seconds / 3600) / 24)

# plot pred and prey for low disturbance
low_pred <- time_radians[low_dist$Species_Name == 'Canis lupus']
low_prey <- time_radians[low_dist$Species_Name == 'Cervus canadensis']

overlapPlot(low_pred, low_prey)
legend('topright', c("Predator", "Prey"), lty=c(1,2), col=c(1,4), bty='n')

overlap7 <- overlapEst(low_pred, low_prey, type="Dhat4")
overlap7

# calculate bootstrap values
bootstrap7 <- bootstrap(low_pred, low_prey, 200, smooth=TRUE, kmax=3, adjust=NA, n.grid=128, type="Dhat4", cores=1)
# now calculate confidence intervals
bootCI(overlap7, bootstrap7, conf = 0.95)

## plot pred and prey overlap for HIGH disturbance

high_dist <- filter(data, Humans_Per_Camera_Per_Day > 0.1)

# convert time into radians
time <- as.POSIXct(high_dist$Local_Time, format = "%H:%M:%S")


# Extract hours, minutes, and seconds
hours <- as.numeric(format(time, "%H"))
minutes <- as.numeric(format(time, "%M"))
seconds <- as.numeric(format(time, "%S"))

# Convert time to radians
time_radians <- 2 * pi * ((hours + minutes / 60 + seconds / 3600) / 24)

# plot pred and prey for high disturbance
high_pred <- time_radians[high_dist$Species_Name == 'Canis lupus']
high_prey <- time_radians[high_dist$Species_Name == 'Cervus canadensis']

overlapPlot(high_pred, high_prey)
legend('topright', c("Predator", "Prey"), lty=c(1,2), col=c(1,4), bty='n')

overlap8 <- overlapEst(high_pred, high_prey, type="Dhat4")
overlap8

# calculate bootstrap values
bootstrap8 <- bootstrap(high_pred, high_prey, 200, smooth=TRUE, kmax=3, adjust=NA, n.grid=128, type="Dhat4", cores=1)
# now calculate confidence intervals
bootCI(overlap8, bootstrap8, conf = 0.95)


# plot histogram of bootstrap values to check for normality
hist(bootstrap7)
hist(bootstrap8)


t.test(bootstrap7, bootstrap8, var.equal = TRUE)

# Elk are crepuscular throughout, but shift toward night rather than morning with higher human presence AND shift toward DAY with higher human presence
# Wolves with high human presence shift from preferring ~6am to a STRONG peak at ~9am and less activity in the evening
# Wolves and elk have greater overlap in areas of LOW human disturbance than areas of HIGH human disturbance

