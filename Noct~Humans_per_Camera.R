# Noct of Pred and Prey ~ Humans_per_trapping_day_per_camera
# Margaret Mercer
# February 14, 2024

library(lme4)
library(tidyr)

data <- read.csv("all_years.csv")

data$Human <- ifelse(data$Species_Name == "Homo sapiens" | data$Species_Name == "Vehicle", 1, 0)
Humans_Per_Camera <- data %>% filter(Human == 1) %>%
  group_by(Site_Name) %>%
  summarise(Humans_Per_Camera = n())
data <- left_join(data, Humans_Per_Camera, by = "Site_Name") %>%
  mutate(Humans_Per_Camera = tidyr::replace_na(Humans_Per_Camera, 0)) # replace nas with 0
data$Humans_Per_Camera_Per_Day <- data$Humans_Per_Camera/data$Survey_Days

# create subset
species <- subset(data, Common_Name == "White-tailed Deer")

prey <- subset(data, Common_Name == "White-tailed Deer")
pred <- subset(data, Common_Name == "Puma")

prey$Pred_Present <- as.numeric(prey$Array %in% pred$Array) # add column to prey dataframe that has predator present as a 0 or 1

summary(glmer(IsNight ~ Humans_Per_Camera_Per_Day + (1 | Year) + (1 | Pred_Present), data = prey, family = binomial))
