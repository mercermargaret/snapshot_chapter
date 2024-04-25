# Human Presence as a function of Latitude?
# Margaret Mercer
# April 23, 2024

library(tidyverse)

data <- read_csv("data/all_years.csv")

plot(data$Humans_Per_Camera_Per_Day ~ data$Latitude)

model <- lm(data$Humans_Per_Camera_Per_Day ~ data$Latitude)

summary(model)

plot(model)
# hm the model looks terrible


# do by camera instead of each observation!!!