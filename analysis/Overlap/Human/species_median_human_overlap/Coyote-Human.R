# Coyote and Human Overlap Analysis
# Margaret Mercer
# May 9, 2024

library(tidyverse)
library(overlap)


data <- read_csv("../data_too_big/all_years.csv")

# no need to subset coyote range...cuz they're everywhere except hawaii :')

# so lets just cut hawaii out lmao
subset <- filter(data, Time_Zone != "Pacific/Honolulu")

# Overlap Analysis ####
species <- filter(subset, Species_Name == 'Canis latrans')

# find median and assign to an object
median <- median(species$Humans_Per_Camera_Per_Day)

# filter TOTAL dataframe to low human disturbance
low_dist <- filter(subset, Humans_Per_Camera_Per_Day < median)

# convert time into radians
time <- as.POSIXct(low_dist$Local_Time, format = "%H:%M:%S")

# Extract hours, minutes, and seconds
hours <- as.numeric(format(time, "%H"))
minutes <- as.numeric(format(time, "%M"))
seconds <- as.numeric(format(time, "%S"))

# Convert time to radians
time_radians <- 2 * pi * ((hours + minutes / 60 + seconds / 3600) / 24)

# plot for low disturbance
low_animal <- time_radians[low_dist$Species_Name == 'Canis latrans']
low_human <- time_radians[low_dist$Species_Name == "Homo sapiens" | low_dist$Species_Name == "Vehicle"]

pdf("results/overlap_graphs/coyote_human_low.pdf")
overlapPlot(low_animal, low_human)
legend('topright', c("Coyote", "Human"), lty=c(1,2), col=c(1,4), bty='n')
dev.off()

overlap_low <- overlapEst(low_animal, low_human, type="Dhat4")
overlap_low

# calculate bootstrap values
bootstrap_low <- bootstrap(low_animal, low_human, 200, smooth=TRUE, kmax=3, adjust=NA, n.grid=128, type="Dhat4", cores=1)
# now calculate confidence intervals
bootCI(overlap_low, bootstrap_low, conf = 0.95)

## plot overlap for HIGH disturbance
high_dist <- filter(subset, Humans_Per_Camera_Per_Day >= median)

# convert time into radians
time <- as.POSIXct(high_dist$Local_Time, format = "%H:%M:%S")

# Extract hours, minutes, and seconds
hours <- as.numeric(format(time, "%H"))
minutes <- as.numeric(format(time, "%M"))
seconds <- as.numeric(format(time, "%S"))

# Convert time to radians
time_radians <- 2 * pi * ((hours + minutes / 60 + seconds / 3600) / 24)

# plot for high disturbance
high_animal <- time_radians[high_dist$Species_Name == 'Canis latrans']
high_human <- time_radians[high_dist$Species_Name == "Homo sapiens" | high_dist$Species_Name == "Vehicle"]

pdf("results/overlap_graphs/coyote_human_high.pdf")
overlapPlot(high_animal, high_human)
legend('topright', c("Coyote", "Human"), lty=c(1,2), col=c(1,4), bty='n')
dev.off()

overlap_high <- overlapEst(high_human, high_animal, type="Dhat4")
overlap_high

# calculate bootstrap values
bootstrap_high <- bootstrap(high_human, high_animal, 200, smooth=TRUE, kmax=3, adjust=NA, n.grid=128, type="Dhat4", cores=1)
# now calculate confidence intervals
bootCI(overlap_high, bootstrap_high, conf = 0.95)


t.test(bootstrap_low, bootstrap_high, var.equal = TRUE)

# plot histogram of bootstrap values?
hist(bootstrap_low)
hist(bootstrap_high)


# Coyote: slight shift to higher noct with higher human presence
# Human: wider spread with higher human activity
# Overlap: slight increase in overlap

# t test results
# data:  bootstrap_low and bootstrap_high
# t = -11.476, df = 398, p-value < 2.2e-16
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#   -0.007209314 -0.005100449
# sample estimates:
#   mean of x mean of y 
# 0.3177057 0.3238606 

# t test cutoff at median of species
# data:  bootstrap_low and bootstrap_high
# t = -13.171, df = 398, p-value < 2.2e-16
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#   -0.007978489 -0.005905981
# sample estimates:
#   mean of x mean of y 
# 0.3151109 0.3220531
