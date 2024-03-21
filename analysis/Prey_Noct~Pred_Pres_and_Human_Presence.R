# Prey IsNight? ~ Pred Presence and Human Disturbance (GLM and SEM)
# Margaret Mercer
# January 2024

data <- read.csv("all_years.csv")
library(dplyr)


# subset species into "prey" and "pred" dataframes.
prey <- subset(data, Species_Name == "Odocoileus virginianus")
pred <- subset(data, Species_Name == "Puma concolor")

# for which arrays of prey was there also a predator present at some point?
prey$Pred_Present <- as.numeric(prey$Array %in% pred$Array) # now I've added a column to the "prey" dataframe with binary coding of predator presence/absence

summary(glm(prey$Is_Night ~ prey$Pred_Present + prey$Humans_Per_Camera_Per_Day, family = binomial))
# 

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
