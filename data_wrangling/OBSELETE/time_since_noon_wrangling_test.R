# Figuring out how to code time since noon
# Margaret Mercer
# March 21, 2024

library(tidyr)
library(readr)
library(lubridate)

data <- read_csv("data/all_years.csv")

data <- separate(data, Local_Date_Time, c("Date", "Time"), sep = " ")

data$Local_Date_Time <- as.POSIXct(paste(data$Date, data$Time), format="%Y-%m-%d %H:%M:%S")
data$noon <- as.POSIXct(paste(data$Date, "12:00:00"), format="%Y-%m-%d %H:%M:%S")

data$difference <- as.numeric(difftime(data$Local_Date_Time, data$noon, units = "mins"))
