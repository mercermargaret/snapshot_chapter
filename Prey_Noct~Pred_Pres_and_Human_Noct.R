# Prey IsNight? ~ Pred Presence and Human Disturbance (GLM and SEM)
# Margaret Mercer
# January 2024

raw2019 <- read.csv("2019.csv")
library(dplyr)


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
