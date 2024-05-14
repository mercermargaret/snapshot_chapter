library(tidyverse)
library(overlap)

data <- read_csv("../data_too_big/all_years.csv")

# convert time into radians
time <- as.POSIXct(data$Local_Time, format = "%H:%M:%S")

# Extract hours, minutes, and seconds
hours <- as.numeric(format(time, "%H"))
minutes <- as.numeric(format(time, "%M"))
seconds <- as.numeric(format(time, "%S"))

# Convert time to radians
time_radians <- 2 * pi * ((hours + minutes / 60 + seconds / 3600) / 24)

# plot pred and prey for low disturbance
time_radians[data$Species_Name == 'Lynx rufus'] %>% 
  densityPlot(rug=TRUE, adjust = 1)