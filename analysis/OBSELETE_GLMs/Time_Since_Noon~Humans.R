# Time Since Noon Analysis
# Margaret Mercer
# March 26, 2024


library(tidyverse)

data <- read_csv("../data_too_big/all_years.csv")

data <- data %>% mutate("Abs_Diff_From_Noon" = abs(Difference_From_Noon))

# pull out predator and prey
prey <- subset(data, Species_Name == "Odocoileus virginianus")
pred <- subset(data, Species_Name == "Puma concolor")

# for which arrays of prey was there also a predator present at some point?
prey$Pred_Present <- as.numeric(prey$Array %in% pred$Array) # now I've added a column to the "prey" dataframe with binary coding of predator presence/absence
prey <- subset(prey, Pred_Present == 1)

plot(Abs_Diff_From_Noon ~ log(Humans_Per_Camera_Per_Day), data = prey)

glm1 <- glm(Abs_Diff_From_Noon ~ Humans_Per_Camera_Per_Day, data = prey, family = quasipoisson)
summary(glm1)
plot(glm1)


# ok so there's an effect but the deviance is HUGE, absolutely IMMENSE. which means we need to use the saturated model?
# deviance divided by sample size is the relevant metric!

plot(Abs_Diff_From_Noon ~ log(Humans_Per_Camera_Per_Day), data = pred)
glm2 <- glm(Abs_Diff_From_Noon ~ Humans_Per_Camera_Per_Day, data = pred, family = quasipoisson)
summary(glm2)
plot(glm2)

# You can't do a zero inflated poission because poisson is for count data

# Fit a Zero-Inflated Gaussian model
# install.packages("glmmTMB")
library(glmmTMB)
zig_model <- glmmTMB(Abs_Diff_From_Noon ~ Humans_Per_Camera_Per_Day, zi=~ Humans_Per_Camera_Per_Day, family = gaussian, data = prey)
# Check the summary of the model
summary(zig_model)
# well this ran, but idk how accurate it is

# transform
plot(Abs_Diff_From_Noon ~ log(Humans_Per_Camera_Per_Day), data = prey)
glm3 <- glm(Abs_Diff_From_Noon ~ Humans_Per_Camera_Per_Day, data = pred, family = gaussian, link = log) 
# this didn't work bc it doesn't like specifying the link function


# let's try offsetting. we'll recreate the # of humans/camera column and then do abs diff from noon as a function of humans per camera, offset by the survey days column
glm(Abs_Diff_From_Noon ~ Humans_Per_Camera + offset(Survey_Days), data = prey, family = poisson)


# ok UGH but the PROBLEM is that it isn't the DATA that's zero inflated, its the EXPLANATORY VARIABLE. UGHGHGHG
# a paper that deals with zero inflated explanatory variable ncbi.nlm.nih.gov/pmc/articles/PMC5628625 but doesn't have any code (at least not in the body of the paper)

# let's take a look at what the data ACTUALLY looks like
test <- subset(data, data$Humans_Per_Camera_Per_Day != 0)
hist(test$Humans_Per_Camera_Per_Day)
test2 <- subset(data, data$Humans_Per_Camera_Per_Day < 3)
hist(test2$Humans_Per_Camera_Per_Day)
test3 <- subset(data, data$Humans_Per_Camera_Per_Day < 1)
hist(test3$Humans_Per_Camera_Per_Day)
test4 <- subset(data, data$Humans_Per_Camera_Per_Day < 0.5)
hist(test4$Humans_Per_Camera_Per_Day)
test5 <- subset(data, data$Humans_Per_Camera_Per_Day < 0.3)
hist(test5$Humans_Per_Camera_Per_Day)
test6 <- subset(data, data$Humans_Per_Camera_Per_Day < 0.3)
hist(test6$Humans_Per_Camera_Per_Day)
# it's not 0 inflated, like I originally thought. It's SLIGHTLY 0 inflated, with a normal curve starting just above 0 and peaking at around 0.04 :') So wtf do I do with that??
