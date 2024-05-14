# Binomial analysis
# Margaret Mercer
# May 2, 2024


library(tidyverse)
library(lme4)

data <- read.csv("../data_too_big/all_years.csv")

species <- subset(data, Species_Name == 'Odocoileus hemionus')


binomial <- species %>%
  group_by(Site_Name) %>%
  summarize(Prop_Noct = mean(Is_Night)) # Calculate binomial proportions for each site
counts <- species %>%
  group_by(Site_Name) %>%
  summarize(Night_Obs = sum(Is_Night == 1),
            Total_Obs = n()) # Create columns for #nightobservations/site and total#observations/site.
props <- merge(counts, binomial, by = "Site_Name") # merge together. now we have a dataframe with each site, the total night observations, total observations, and the proportion of the two.


# now I want to add a "humans per camera per day" column to that (I still want only one row per camera)
unique_species <- species %>% distinct(Site_Name, .keep_all = TRUE)
unique_species <- unique_species %>% select(Site_Name, Humans_Per_Camera_Per_Day)
props_updated <- props %>% 
  left_join(unique_species, by = "Site_Name")


# plot(props_updated$Prop_Noct ~ logit(props_updated$Humans_Per_Camera_Per_Day))
# summary(glm(props_updated$Prop_Noct ~ log(props_updated$Humans_Per_Camera_Per_Day)))


# # Y/m
# 
# Y <- props_updated$Night_Obs
# m <- props_updated$Total_Obs
# 
# logit <- log((Y+0.5)/(m - Y + 0.5))
# plot(logit ~ log(props_updated$Humans_Per_Camera_Per_Day)) # this works too
# 
# # ugh idk why a glm won't work. I mean i know why, it doesn't like that I'm logging a 0, but idk how to fix it
# 
# 
# 

# what if I split it into low/high and see what proportion of occurrences are at night between the two?
overall_noct <- sum(props_updated$Night_Obs)/sum(props_updated$Total_Obs)

low <- filter(props_updated, Humans_Per_Camera_Per_Day < median(props_updated$Humans_Per_Camera_Per_Day))
high <- filter(props_updated, Humans_Per_Camera_Per_Day >= median(props_updated$Humans_Per_Camera_Per_Day))

low_noct <- sum(low$Night_Obs)/sum(low$Total_Obs)
high_noct <- sum(high$Night_Obs)/sum(high$Total_Obs)

