# Overlap Analysis
# Margaret Mercer
# March 13, 2024

# https://cran.r-project.org/web/packages/overlap/overlap.pdf

library(tidyverse)
library(overlap)


data <- read_csv("data/all_years.csv")

data <- separate(data, Local_Date_Time, c("Date", "Time"), sep = " ")


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
time <- as.POSIXct(data$Time, format = "%H:%M:%S")
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


# low vs high human disturbance estimates ####
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

overlap3 <- overlapEst(low_pred, low_prey, type="Dhat4")
overlap3

# calculate bootstrap values
bootstrap3 <- bootstrap(low_pred, low_prey, 200, smooth=TRUE, kmax=3, adjust=NA, n.grid=128, type="Dhat4", cores=1)
# now calculate confidence intervals
bootCI(overlap3, bootstrap3, conf = 0.95)

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

overlap4 <- overlapEst(high_pred, high_prey, type="Dhat4")
overlap4

# calculate bootstrap values
bootstrap4 <- bootstrap(high_pred, high_prey, 200, smooth=TRUE, kmax=3, adjust=NA, n.grid=128, type="Dhat4", cores=1)
# now calculate confidence intervals
bootCI(overlap4, bootstrap4, conf = 0.95)


t.test(bootstrap3, bootstrap4)
# ok so there's definitely a difference between the two; but is a t test the right method?

