# Getting a dataframe of one line per camera
# Margaret Mercer
# May 7, 2024

library(tidyverse)

data <- read.csv("../data_too_big/all_years.csv")

cameras <- data %>% distinct(Site_Name, .keep_all = TRUE)

cameras <- select(cameras, Array:Longitude, Time_Zone, Year, Humans_Per_Camera, Humans_Per_Camera_Per_Day)

write_csv(cameras, "data/cameras.csv")
