# Preliminary Analyses with Subsets
# Margaret Mercer
# Dec 14, 2023

options(digits = 3)

## general glms

#try a basic glm first to see if theres an effect for animals in general
# (side note, Jesse said this probably won't show an effect since the species that are more tolerant of humans will "fill in" the place of the species less tolerant)
# so lets delete vehicle, no animal, human non-staff, domestic cow, domestic horse, domestic sheep, camera trapper, bicycle
onlyanimals_19 <- new_19[new_19$Common_Name != "Vehicle" 
                         & new_19$Common_Name != "No Animal" 
                         & new_19$Common_Name != "Human non-staff" 
                         & new_19$Common_Name != "Domestic Cow"
                         & new_19$Common_Name != "Domestic Horse"
                         & new_19$Common_Name != "Domestic Sheep" 
                         & new_19$Common_Name != "Camera Trapper" 
                         & new_19$Common_Name != "Bicycle", ]

# now a basic glm of deer nocturnality as a function of human disturbance
deer <- subset(new_19, Common_Name == "White-tailed Deer")
deerglm <- glm(deer$IsNight ~ deer$Disturbance, family = binomial)
summary(deerglm) # just right off the bat it looks like as disturbance increases, odds of night occurrence also increases...by 0.14 (logit scale)
deerglm$deviance/deerglm$df.residual # so dispersion looks good...
pchisq(deerglm$deviance, df=deerglm$df.residual, lower.tail=FALSE) # but I got a 0 here for the goodness of fit test so idk what that means

# let's see what species are around in 2019 (side note, there's no elk in 2019?? but a ton in 2020? v weird)
species_counts <- table(obs_2019$Common_Name)



## creating

# subset moose and wolf into "prey" and "pred" dataframes.
prey <- subset(new_19, Species_Name == "Alces alces")
pred <- subset(new_19, Species_Name == "Canis lupus")

# for which deployments of prey was there also a predator present at some point?
prey$Pred_Present <- as.numeric(prey$Site_Name %in% pred$Site_Name)
# now I've added a column to the "prey" dataframe with binary coding of predator presence/absence

# now I need to figure out the night/day ratio for each deployment ID of wolves
summary(glm(m$IsNight ~ prey$Pred_Present + prey$Disturbance, family = binomial))
# ugh so no effect of predator presence or human disturbance on prey nocturnality...great

# so we have "pred"
str(pred)
# and we have nocturnality
pred$IsNight
# so for each site name, we need the proportion of pred night/day

pred_noct_by_site <- pred %>%
  group_by(Site_Name) %>%
  summarize(
    Pred_Noct_By_Site = mean(IsNight == 1)
  )

# Merge proportions back to the original dataframe, keeping all rows from the original dataframe
pred_with_noct <- merge(pred, pred_noct_by_site, by = "Site_Name", all.x = TRUE)

# Now I add a column to the "prey" dataframe with the proportion of predator nocturnality
prey_with_pred_noct <- merge(prey, pred_noct_by_site, by = "Site_Name", all.x = TRUE)




## SEM using lavaan

# install.packages(lavaan)
library(lavaan)

myModel <- ' 
 # regressions
   IsNight ~ Disturbance + Pred_Present
'

fit <- sem(model = myModel, 
           data = prey_with_pred_noct) 
summary(fit)

# so it looks like there's not an effect? Of either disturbance or predator presence?



## here's the idea Javan gave me: Prey Nocturnality (probability) ~ -1 + Predator_Absence*beta_A + Predator_Presence * beta_P + Predator_Prop:Predator_Presence * beta_P_prop + Disturbance * beta_D + Disturbance:Predator_Presence * beta_D_P + Disturbance:Predator_Prop:Predator_Presence * beta_D_P_prop 

prey_with_pred_noct$Pred_Absent <- ifelse(prey_with_pred_noct$Pred_Present == 0, 1, 0)

summary(glm(data = prey_with_pred_noct, IsNight ~ -1 + Pred_Absent + Pred_Present + Pred_Noct_By_Site:Pred_Present + Disturbance + Disturbance:Pred_Present + Disturbance:Pred_Noct_By_Site:Pred_Present))



## Prey noct as a function of human presence when predator present vs absent
# create predator and prey datasets
prey <- subset(new_19, Common_Name == "White-tailed Deer")
pred <- subset(new_19, Common_Name == "Puma")

# subset prey into "with pred" and "without pred"
withpred <- subset(prey, Pred_Present == 1)
wopred <- subset(prey, Pred_Present == 0)

# glms
summary(glm(pred$IsNight ~ pred$Disturbance)) # predator response to human presence
summary(glm(withpred$IsNight ~ withpred$Disturbance)) # prey response to human presence with predators around
summary(glm(wopred$IsNight ~ wopred$Disturbance)) # prey response to human presence without predators around

# with coyotes and red fox, no effect of disturbance on coyote activity, strong response to human disturbance of 
# red fox when coyotes present, and no response to human disturbance of red fox when coyotes not present

# Hm no strong effect for gray foxes or gray squirrels....

# Or for puma. But strong negative effect of disturbance on deer with predators present, and positive effect of 
# disturbance on deer with predators absent



## Now let's create data of proportions rather than binomial. Do this with a subset of ONE species! (if it's predator, replace "prey" with "pred")

# Calculate binomial proportions for each site
binomial <- prey %>%
  group_by(Site_Name) %>%
  summarize(Prop_Noct = mean(IsNight))

# Create columns for #nightobservations/site and total#observations/site.
counts <- prey %>%
  group_by(Site_Name) %>%
  summarize(Night_Obs = sum(IsNight == 1),
            Total_Obs = n())


# merge together. now we have a dataframe with each site, the total night observations, total observations, and the proportion of the two.
counts_props <- merge(counts, binomial, by = "Site_Name")

# cool. now we'll merge that back into the dataframe with all prey observations:
prey_with_props <- merge(prey, counts_props, by = "Site_Name")

# now pre_with_probs is all observations of a single species, with binomial proportions as counts and proportion


## analysis of binomial counts/proportions
# let's visualize...starting with the reduced (per site) dataframe bc is there even a reason to have each individual observation right now??
# we'll need to add the human disturbance column to the counts_probs dataframe first

# first I'm creating a "dist" dataframe where each site name gets a line and its disturbance value.
dist <- prey %>%
  group_by(Site_Name) %>%
  summarize(Disturbance = mean(Disturbance))

# now merge that dataframe with the per site dataframe so for our chosen species, we have a dataframe with one row per site
# and the proportions of nocturnality and the human disturbance for that site
props_by_site <- merge(counts_props, dist, by = "Site_Name")

plot(Prop_Noct ~ Disturbance, data = props_by_site)





