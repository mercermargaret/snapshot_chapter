# Preliminary Analyses with Subsets
# Margaret Mercer
# Dec 14, 2023

## glms

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

# now a glm of deer nocturnality as a function of human disturbance
deer <- subset(new_19, Common_Name == "White-tailed Deer")
deerglm <- glm(deer$IsNight ~ deer$Disturbance, family = binomial)
summary(deerglm) # just right off the bat it looks like as disturbance increases, odds of night occurrence also increases...by 0.14 (logit scale)
deerglm$deviance/deerglm$df.residual # so dispersion looks good...
pchisq(deerglm$deviance, df=deerglm$df.residual, lower.tail=FALSE) # but I got a 0 here for the goodness of fit test so idk what that means

# let's see what species are around in 2019 (side note, there's no elk in 2019?? but a ton in 2020? v weird)
species_counts <- table(obs_2019$Common_Name)

# let's do moose and gray wolf (Alces alces and Canis lupus)
# subset moose and gray wolf into one dataframe
mw <- subset(new_19, Species_Name == "Alces alces" | Species_Name == "Canis lupus")

# logistic regression of nocturnality of wolves and moose as a function of disturbance
logregmw <- glm(mw$IsNight ~ mw$Disturbance, family = binomial)
summary(logregmw)



## wrangling time!!

# subset moose and wolf into "prey" and "pred" dataframes. Substitute different species here and you can perform the rest of the analysis as written
prey <- subset(new_19, Species_Name == "Alces alces")
pred <- subset(new_19, Species_Name == "Canis lupus")

# for which deployments of prey was there also a predator present at some point?
prey$Site_Name # here's all the deployments where there were prey
pred$Site_Name # and here's all the deployments where there were predators
# which values of prey are also present in pred?
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

proportion_by_site <- pred %>%
  group_by(Site_Name) %>%
  summarize(
    Pred_Noct_By_Site = mean(IsNight == 1)
  )

# Merge proportions back to the original dataframe, keeping all rows from the original dataframe
pred_with_props <- merge(pred, proportion_by_site, by = "Site_Name", all.x = TRUE)

# Now I add a column to the "prey" dataframe with the proportion of predator nocturnality
prey_with_props <- merge(prey, proportion_by_site, by = "Site_Name", all.x = TRUE)



## and some analysis using lavaan

# install.packages(lavaan)
library(lavaan)

myModel <- ' 
 # regressions
   IsNight ~ Disturbance + Pred_Present 
'

fit <- sem(model = myModel, 
           data = prey_with_props) 
summary(fit)

# so it looks like there's not an effect? Of either disturbance or predator presence?
