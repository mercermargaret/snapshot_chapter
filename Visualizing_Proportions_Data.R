# Visualize Binomial Proportions Data
# Margaret Mercer
# January 24, 2024

# BIG problem!! Disturbance has NAs in it; figure out WHY!!! --> Jesse thinks it's because of water


data <- read.csv("all_years.csv")
library(dplyr)
library(ggplot2)

# bin prey data as a function of human disturbance

prey <- subset(data, Common_Name == "Eastern Cottontail")
binomial <- prey %>%
  group_by(Site_Name) %>%
  summarize(Prop_Noct = mean(IsNight)) # Calculate binomial proportions for each site
counts <- prey %>%
  group_by(Site_Name) %>%
  summarize(Night_Obs = sum(IsNight == 1),
            Total_Obs = n()) # Create columns for #nightobservations/site and total#observations/site.
counts_props <- merge(counts, binomial, by = "Site_Name") # merge together. now we have a dataframe with each site, the total night observations, total observations, and the proportion of the two.
prey <- merge(prey, counts_props, by = "Site_Name") # cool. now we'll merge that back into the dataframe with all prey observations

# generic code for binning
df <-  prey %>% mutate(bin = ntile(Prop_Noct, n=10))
new_df <-  df %>% group_by(bin) %>% summarise(Disturbance = mean(Disturbance, na.rm = TRUE), Prop_Noct = mean(Prop_Noct)) #find the x and y mean of each bin
ggplot(new_df, aes(x=Disturbance, y=Prop_Noct)) + 
  geom_point() +
  ggtitle("Prey Nocturnality as a Function of Human Disturbance")

# let's try for predator data...
pred <- subset(data, Common_Name == "Red Fox")
binomial <- pred %>%
  group_by(Site_Name) %>%
  summarize(Prop_Noct = mean(IsNight))
counts <- pred %>%
  group_by(Site_Name) %>%
  summarize(Night_Obs = sum(IsNight == 1),
            Total_Obs = n())
counts_props <- merge(counts, binomial, by = "Site_Name")
pred <- merge(pred, counts_props, by = "Site_Name")

# now visualize predator noct as function of human disturbance
df <-  pred %>% mutate(bin = ntile(Prop_Noct, n=10))
new_df <-  df %>% group_by(bin) %>% summarise(Disturbance = mean(Disturbance, na.rm = TRUE), Prop_Noct = mean(Prop_Noct)) #find the x and y mean of each bin
ggplot(new_df, aes(x=Disturbance, y=Prop_Noct)) + 
  geom_point() +
  ggtitle("Predator Nocturnality as a Function of Human Disturbance")






# is the following even necessary?? Does it actually make a difference? Some preds will escape detection; also pred/prey interactions not always linear. Other preds may be impacting other prey.
# visualize prey noct w vs w/o predators

pred <- dplyr::select(pred, -Date_Time, -IsNight, -Count, -Altitude) # get rid of columns that distinguish individual observations
pred <- unique(pred) # Remove duplicated rows so now there's only one row per camera
prey$Pred_Present <- as.numeric(prey$Array %in% pred$Array) # add column to prey dataframe that has predator present as a 0 or 1
# subset prey into "with pred" and "without pred"
withpred <- subset(prey, Pred_Present == 1)
wopred <- subset(prey, Pred_Present == 0)

df <-  withpred %>% mutate(bin = ntile(Prop_Noct, n=10))
new_df <-  df %>% group_by(bin) %>% summarise(Disturbance = mean(Disturbance, na.rm = TRUE), Prop_Noct = mean(Prop_Noct)) #find the x and y mean of each bin
ggplot(new_df, aes(x=Disturbance, y=Prop_Noct)) + 
  geom_point() +
  ggtitle("Prey Nocturnality When Predator is Present")

df <-  wopred %>% mutate(bin = ntile(Prop_Noct, n=10))
new_df <-  df %>% group_by(bin) %>% summarise(Disturbance = mean(Disturbance, na.rm = TRUE), Prop_Noct = mean(Prop_Noct)) #find the x and y mean of each bin
ggplot(new_df, aes(x=Disturbance, y=Prop_Noct)) + 
  geom_point() +
  ggtitle("Prey Nocturnality When Predator is Absent")

 