# Figuring out what's wrong with suncalc and why the altitudes are all wonky
# Margaret Mercer
# Feburary 19, 2024

# THIS IS VERY USEFUL! https://cran.r-project.org/web/packages/lutz/lutz.pdf

library(tidyr)
library(ggplot2)
# install.packages("lutz")
library(lutz)
library(lubridate)
library(forcats)

data <- read.csv("2021.csv")

data <- separate(data, Local_Date_Time, c("Date", "Time"), sep = " ")

ggplot(data, aes(x = Time, y = Altitude)) +
  geom_point()

# FUCK YEEEEEEEAH