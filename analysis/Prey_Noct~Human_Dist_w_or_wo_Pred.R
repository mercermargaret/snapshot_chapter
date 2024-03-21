# Prey Noct ~ Human Disturbance with a. Predator Present and b. Predator Absent
# Margaret Mercer
# January 2024

data <- read.csv("all_years.csv")
library(lme4)
library(dplyr)

species_counts <- table(data$Common_Name)

# create predator and prey datasets
prey <- subset(data, Common_Name == "White-tailed Deer")
pred <- subset(data, Common_Name == "Puma")

prey$Pred_Present <- as.numeric(prey$Array %in% pred$Array) # add column to prey dataframe that has predator present as a 0 or 1

# subset prey into "with pred" and "without pred"
withpred <- subset(prey, Pred_Present == 1)
wopred <- subset(prey, Pred_Present == 0)

# glms
summary(glmer(IsNight ~ Disturbance + (1 | Year), data = pred, family = binomial)) # predator response to human presence
summary(glmer(IsNight ~ Disturbance + (1 | Year), data = withpred, family = binomial)) # prey response to human presence with predators around
summary(glmer(IsNight ~ Disturbance + (1 | Year), data = wopred, family = binomial)) # prey response to human presence without predators around

# PROBLEM it's pulling up NAs when I use year as a random effect ugh

# 2019:
# No effect for puma. Or for deer when puma present.
# BUT when puma were absent, white tailed deer became more nocturnal with increased human presence!!
# Coyotes and red fox: no effect for coyotes, increased nocturnality of red fox with human presence when coyotes present,
# and no appreciable effect of human presence on red fox nocturnality when coyotes absent
# Hm no strong effect for gray foxes or gray squirrels... or for gray wolves and moose.

# all years:
# 
