# Binomial Counts and Proportions: Wrangling and GLMs
# Margaret Mercer
# January 2024

data <- read.csv("../data_too_big/all_years.csv")
library(dplyr)


prey <- subset(data, Common_Name == "White-tailed Deer")
binomial <- prey %>%
  group_by(Site_Name) %>%
  summarize(Prop_Noct = mean(Is_Night)) # Calculate binomial proportions for each site
counts <- prey %>%
  group_by(Site_Name) %>%
  summarize(Night_Obs = sum(Is_Night == 1),
            Total_Obs = n()) # Create columns for #nightobservations/site and total#observations/site.
counts_props <- merge(counts, binomial, by = "Site_Name") # merge together. now we have a dataframe with each site, the total night observations, total observations, and the proportion of the two.
prey <- merge(prey, counts_props, by = "Site_Name") # cool. now we'll merge that back into the dataframe with all prey observations

plot(prey$Prop_Noct ~ log(prey$Humans_Per_Camera_Per_Day))

# This is all commented out because it was a different way of creating the reduced dataframe but it didn't preserve all the original columns
# dist <- prey %>%
#   group_by(Site_Name) %>%
#   summarize(Disturbance = mean(Disturbance)) # first I'm creating a "dist" dataframe where each site name gets a line and its disturbance value.
# prey <- merge(counts_props, dist, by = "Site_Name") # now merge that dataframe with the per site dataframe so for our chosen species, we have a dataframe with one row per site and the proportions of nocturnality and the human disturbance for that site

glm(Prop_Noct ~ Disturbance, data = prey)

prey <- dplyr::select(prey, -Date_Time, -IsNight, -Count, -Altitude) # get rid of columns that distinguish individual observations
prey <- unique(prey) # Remove duplicated rows so there's only one row per camera

# creating binomial counts and proportions for predator now
pred <- subset(data, Common_Name == "Puma")
binomial <- pred %>%
  group_by(Site_Name) %>%
  summarize(Prop_Noct = mean(IsNight))
counts <- pred %>%
  group_by(Site_Name) %>%
  summarize(Night_Obs = sum(IsNight == 1),
            Total_Obs = n())
counts_props <- merge(counts, binomial, by = "Site_Name")
pred <- merge(pred, counts_props, by = "Site_Name")

# This is all commented out because it was a different way of creating the reduced dataframe but it didn't preserve all the original columns
# dist <- pred %>%
#   group_by(Site_Name) %>%
#   summarize(Disturbance = mean(Disturbance))
# pred <- merge(counts_props, dist, by = "Site_Name")

pred <- dplyr::select(pred, -Date_Time, -IsNight, -Count, -Altitude) # get rid of columns that distinguish individual observations
pred <- unique(pred) # Remove duplicated rows so now there's only one row per camera

# now prepare predator and prey dataframes and run glms on them, just like above, but now we're using binomial proportions rather than just binary data

prey$Pred_Present <- as.numeric(prey$Array %in% pred$Array) # add column to prey dataframe that has predator present as a 0 or 1

# subset prey into "with pred" and "without pred"
withpred <- subset(prey, Pred_Present == 1)
wopred <- subset(prey, Pred_Present == 0)

# glms
summary(glm(pred$Prop_Noct ~ pred$Disturbance)) # predator response to human presence
summary(glm(withpred$Prop_Noct ~ withpred$Disturbance)) # prey response to human presence with predators around
summary(glm(wopred$Prop_Noct ~ wopred$Disturbance)) # prey response to human presence without predators around

# hm so the problem with doing it like this is that now the number of observations is way down to the number of sites, not just the number of individuals. Ugh.





