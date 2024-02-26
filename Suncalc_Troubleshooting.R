# Figuring out what's wrong with suncalc and why the altitudes are all wonky
# Margaret Mercer
# Feburary 19, 2024

# THIS IS VERY USEFUL! https://cran.r-project.org/web/packages/lutz/lutz.pdf

data <- read.csv("2019.csv")
library(tidyr)
library(ggplot2)
# install.packages("lutz")
library(lutz)
library(lubridate)
library(forcats)

ggplot(data, aes(x = Time, y = Altitude)) +
  geom_point()

data$Time_Zone <- tz_lookup_coords(data$Latitude, data$Longitude, method = "accurate", warn = TRUE)

# Convert to POSIXct object
data$datetime <- as.POSIXct(strptime(data$datetime, format = "%YYYY-%MM-%DD %H:%M:%S"))

# Convert time zones to UTC
data$datetime_utc <- with_tz(data$datetime, tzone = data$Time_Zone)
# cool except this isn't right ugh

with_tz(data$datetime, tzone = data$Time_Zone)