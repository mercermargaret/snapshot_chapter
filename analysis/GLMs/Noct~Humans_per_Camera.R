# Noct of Pred and Prey ~ Humans_per_trapping_day_per_camera
# Margaret Mercer
# February 14, 2024

library(lme4)
library(tidyr)

data <- read_csv("../data_too_big/all_years.csv")

prey <- subset(data, Common_Name == "White-tailed Deer")
pred <- subset(data, Common_Name == "Puma")

prey$Pred_Present <- as.numeric(prey$Array %in% pred$Array) # add column to prey dataframe that has predator present as a 0 or 1
Predators_Per_Array <- prey %>% filter(Pred_Present == 1) %>%
  group_by(Site_Name) %>%
  summarise(Predators_Per_Array = n())
prey <- left_join(prey, Predators_Per_Array, by = "Site_Name") %>%
  mutate(Predators_Per_Array = tidyr::replace_na(Predators_Per_Array, 0)) # replace nas with 0
prey$Predators_Per_Array_Per_Day <- prey$Predators_Per_Array/prey$Survey_Days
prey <- subset(prey, select = c(-Pred_Present, -Predators_Per_Array))


summary(glmer(Is_Night ~ Humans_Per_Camera_Per_Day + Predators_Per_Array_Per_Day + (Humans_Per_Camera_Per_Day*Predators_Per_Array_Per_Day) + (1 | Year), data = prey, family = binomial))
summary(glmer(Is_Night ~ Humans_Per_Camera_Per_Day + (1 | Year), data = pred, family = binomial))


