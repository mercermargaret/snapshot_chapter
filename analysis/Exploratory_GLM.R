# Exploratory GLMs on whole dataset
# Margaret Mercer
# Dec 14, 2023

raw2019 <- read.csv("2019.csv")
options(digits = 3)
library(dplyr)

# Exploratory GLMs

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


