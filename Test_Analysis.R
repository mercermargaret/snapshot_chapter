# Preliminary Analyses with Subsets
# Margaret Mercer
# Dec 14, 2023

# Each of the following is an alternate analysis option. Each can be run independently using "2019" data.

raw2019 <- read.csv("2019.csv")
options(digits = 3)
library(dplyr)

# Exploratory GLMs ####

#try a basic glm first to see if theres an effect for animals in general
# (side note, Jesse said this probably won't show an effect since the species that are more tolerant of humans will "fill in" the place of the species less tolerant)
# so lets delete vehicle, no animal, human non-staff, domestic cow, domestic horse, domestic sheep, camera trapper, bicycle
onlyanimals_19 <- raw2019[raw2019$Common_Name != "Vehicle" 
                         & raw2019$Common_Name != "No Animal" 
                         & raw2019$Common_Name != "Human non-staff" 
                         & raw2019$Common_Name != "Domestic Cow"
                         & raw2019$Common_Name != "Domestic Horse"
                         & raw2019$Common_Name != "Domestic Sheep" 
                         & raw2019$Common_Name != "Camera Trapper" 
                         & raw2019$Common_Name != "Bicycle", ]

# now a basic glm of deer nocturnality as a function of human disturbance
deer <- subset(raw2019, Common_Name == "White-tailed Deer")
deerglm <- glm(deer$IsNight ~ deer$Disturbance, family = binomial)
summary(deerglm) # just right off the bat it looks like as disturbance increases, odds of night occurrence also increases...by 0.14 (logit scale)
deerglm$deviance/deerglm$df.residual # so dispersion looks good...
pchisq(deerglm$deviance, df=deerglm$df.residual, lower.tail=FALSE) # but I got a 0 here for the goodness of fit test so idk what that means

# let's see what species are around in 2019 (side note, there's no elk in 2019?? but a ton in 2020? v weird)
species_counts <- table(obs_2019$Common_Name)



# Prey IsNight? ~ Pred Presence and Human Disturbance (GLM and SEM) ####

# subset species into "prey" and "pred" dataframes.
prey <- subset(raw2019, Species_Name == "Alces alces")
pred <- subset(raw2019, Species_Name == "Canis lupus")

# for which arrays of prey was there also a predator present at some point?
prey$Pred_Present <- as.numeric(prey$Array %in% pred$Array) # now I've added a column to the "prey" dataframe with binary coding of predator presence/absence

summary(glm(prey$IsNight ~ prey$Pred_Present + prey$Disturbance, family = binomial))
# ugh so no effect of predator presence or human disturbance on prey nocturnality...great

# install.packages(lavaan)
library(lavaan)
myModel <- ' 
 # regressions
   IsNight ~ Disturbance + Pred_Present
'
fit <- sem(model = myModel, 
           data = prey) 
summary(fit)
# so it looks like there's not an effect? Of either disturbance or predator presence?


# Add Column to Prey Dataframe with Pred Noct Proportion ####
prey <- subset(raw2019, Species_Name == "Alces alces")
pred <- subset(raw2019, Species_Name == "Canis lupus") # subset

# get proportion of predator nocturnality for each site name
pred_noct_by_site <- pred %>%
  group_by(Site_Name) %>%
  summarize(
    Pred_Noct_By_Site = mean(IsNight == 1)
  )

pred <- merge(pred, pred_noct_by_site, by = "Site_Name", all.x = TRUE) # Merge proportions back to the original dataframe, keeping all rows from the original dataframe

prey <- merge(prey, pred_noct_by_site, by = "Site_Name", all.x = TRUE) # Now I add a column to the "prey" dataframe with the proportion of predator nocturnality


# Javaan's model (takes into account pred presence AND pred nocturnality) ####
# Prey Nocturnality (probability) ~ -1 + Predator_Absence*beta_A + Predator_Presence * beta_P + Predator_Prop:Predator_Presence * beta_P_prop + Disturbance * beta_D + Disturbance:Predator_Presence * beta_D_P + Disturbance:Predator_Prop:Predator_Presence * beta_D_P_prop 

# add column to prey dataframe with pred noct by site
prey <- subset(raw2019, Species_Name == "Alces alces")
pred <- subset(raw2019, Species_Name == "Canis lupus") # subset
pred_noct_by_site <- pred %>%
  group_by(Site_Name) %>%
  summarize(
    Pred_Noct_By_Site = mean(IsNight == 1)
  ) # get proportion of predator nocturnality for each site name
pred <- merge(pred, pred_noct_by_site, by = "Site_Name", all.x = TRUE) # Merge proportions back to the original dataframe, keeping all rows from the original dataframe
prey <- merge(prey, pred_noct_by_site, by = "Site_Name", all.x = TRUE) # Now I add a column to the "prey" dataframe with the proportion of predator nocturnality

# add pred present and pred absent columns to prey dataframe
prey$Pred_Present <- as.numeric(prey$Array %in% pred$Array)
prey$Pred_Absent <- ifelse(prey$Pred_Present == 0, 1, 0)

summary(glm(data = prey, IsNight ~ -1 + Pred_Absent + Pred_Present + Pred_Noct_By_Site:Pred_Present + Disturbance + Disturbance:Pred_Present + Disturbance:Pred_Noct_By_Site:Pred_Present))



# Prey Noct ~ Human Disturbance with a. Predator Present and b. Predator Absent ####
# create predator and prey datasets
prey <- subset(raw2019, Common_Name == "White-tailed Deer")
pred <- subset(raw2019, Common_Name == "Puma")

prey$Pred_Present <- as.numeric(prey$Array %in% pred$Array) # add column to prey dataframe that has predator present as a 0 or 1

# subset prey into "with pred" and "without pred"
withpred <- subset(prey, Pred_Present == 1)
wopred <- subset(prey, Pred_Present == 0)

# glms
summary(glm(pred$IsNight ~ pred$Disturbance)) # predator response to human presence
summary(glm(withpred$IsNight ~ withpred$Disturbance)) # prey response to human presence with predators around
summary(glm(wopred$IsNight ~ wopred$Disturbance)) # prey response to human presence without predators around

# No effect for puma. Or for deer when puma present.
# BUT when puma were absent, white tailed deer became more nocturnal with increased human presence!!

# Coyotes and red fox: no effect for coyotes, increased nocturnality of red fox with human presence when coyotes present,
# and no appreciable effect of human presence on red fox nocturnality when coyotes absent

# Hm no strong effect for gray foxes or gray squirrels... or for gray wolves and moose.



# Binomial Counts and Proportions: Wrangling and GLMs ####

prey <- subset(raw2019, Common_Name == "White-tailed Deer")
binomial <- prey %>%
  group_by(Site_Name) %>%
  summarize(Prop_Noct = mean(IsNight)) # Calculate binomial proportions for each site
counts <- prey %>%
  group_by(Site_Name) %>%
  summarize(Night_Obs = sum(IsNight == 1),
            Total_Obs = n()) # Create columns for #nightobservations/site and total#observations/site.
counts_props <- merge(counts, binomial, by = "Site_Name") # merge together. now we have a dataframe with each site, the total night observations, total observations, and the proportion of the two.
prey <- merge(prey, counts_props, by = "Site_Name") # cool. now we'll merge that back into the dataframe with all prey observations

# This is all commented out because it was a different way of creating the reduced dataframe but it didn't preserve all the original columns
# dist <- prey %>%
#   group_by(Site_Name) %>%
#   summarize(Disturbance = mean(Disturbance)) # first I'm creating a "dist" dataframe where each site name gets a line and its disturbance value.
# prey <- merge(counts_props, dist, by = "Site_Name") # now merge that dataframe with the per site dataframe so for our chosen species, we have a dataframe with one row per site and the proportions of nocturnality and the human disturbance for that site

plot(Prop_Noct ~ Disturbance, data = prey) # no clear visual patterns in white tailed deer nocturnality as a response to disturbance
glm(Prop_Noct ~ Disturbance, data = prey)

prey <- dplyr::select(prey, -Date_Time, -IsNight, -Count, -Altitude) # get rid of columns that distinguish individual observations
prey <- unique(prey) # Remove duplicated rows so there's only one row per camera

# creating binomial counts and proportions for predator now
pred <- subset(raw2019, Common_Name == "Puma")
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





# Different Metrics for Human Activity ####
## Humans/trapping day/camera
# group by site, filter for humans, summarize number of humans per site, for each site divide by number of survey days

## Human there in last 24 hours? yes/no


## Time since last human


## Distance to closest road


## Human density, county population



