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
data <- left_join(data, Humans_Per_Camera, by = "Site_Name") %>%
  mutate(Humans_Per_Camera = tidyr::replace_na(Humans_Per_Camera, 0)) # replace nas with 0
data$Humans_Per_Camera_Per_Day <- data$Humans_Per_Camera/data$Survey_Days
# we got one NA for Humans_Per_Camera_Per_Day, because one "survey days" was 0 (line 330945, NY_Forest_Paul_Smiths_College_20_12) (line 377947 also has 0 survey days, VA_Forest_SCBI_20_15)
# turn them into ones! Is that allowed?

## Human there in last 24 hours? yes/no


## Time since last human


## Distance to closest road


## Human density, county population



