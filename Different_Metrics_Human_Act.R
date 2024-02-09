# Different Metrics for Human Activity
# Margaret Mercer
# January 2024

data <- read.csv("all_years.csv")
library(dplyr)

## Humans/trapping day/camera
# create new column with 1 for human or vehicle
data$Human <- ifelse(data$Species_Name == "Homo sapiens" | data$Species_Name == "Vehicle", 1, 0)
Humans_Per_Camera <- data %>% filter(Human == 1) %>%
  group_by(Site_Name) %>%
  summarise(Humans_Per_Camera = n())
data <- merge(data, Humans_Per_Camera, by = "Site_Name")
data$Humans_Per_Camera_Per_Day <- data$Humans_Per_Camera/data$Survey_Days


## Human there in last 24 hours? yes/no


## Time since last human


## Distance to closest road


## Human density, county population



