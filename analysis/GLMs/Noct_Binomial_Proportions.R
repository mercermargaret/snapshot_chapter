# Binomial analysis of nocturnality
# Margaret Mercer
# May 2, 2024
# (visualize this in "Nocturnality_Visualization")


library(tidyverse)
library(lme4)

data <- read.csv("../data_too_big/all_years.csv")

species <- subset(data, Species_Name == 'Homo sapiens')


binomial <- species %>%
  group_by(Site_Name) %>%
  summarize(Prop_Noct = mean(Is_Night)) # Calculate binomial proportions for each site
counts <- species %>%
  group_by(Site_Name) %>%
  summarize(Night_Obs = sum(Is_Night == 1),
            Total_Obs = n()) # Create columns for #nightobservations/site and total#observations/site.
props <- merge(counts, binomial, by = "Site_Name") # merge together. now we have a dataframe with each site, the total night observations, total observations, and the proportion of the two.


# now I want to add "humans per camera per day" column to that (I still want only one row per camera)
unique_species <- species %>% distinct(Site_Name, .keep_all = TRUE)
unique_species <- unique_species %>% dplyr::select(Site_Name, Humans_Per_Camera_Per_Day)
props_updated <- props %>% 
  left_join(unique_species, by = "Site_Name")


# plot(props_updated$Prop_Noct ~ logit(props_updated$Humans_Per_Camera_Per_Day))
# summary(glm(props_updated$Prop_Noct ~ log(props_updated$Humans_Per_Camera_Per_Day)))


# # Y/m
# 
# Y <- props_updated$Night_Obs
# m <- props_updated$Total_Obs
# 
# logit <- log((Y+0.5)/(m - Y + 0.5))
# plot(logit ~ log(props_updated$Humans_Per_Camera_Per_Day)) # this works too
# 
# # ugh idk why a glm won't work. I mean i know why, it doesn't like that I'm logging a 0, but idk how to fix it





# what if I split it into low/high and see what proportion of occurrences are at night between the two?
overall_noct <- sum(props_updated$Night_Obs)/sum(props_updated$Total_Obs)

low <- filter(props_updated, Humans_Per_Camera_Per_Day < median(props_updated$Humans_Per_Camera_Per_Day))
high <- filter(props_updated, Humans_Per_Camera_Per_Day >= median(props_updated$Humans_Per_Camera_Per_Day))

low_noct <- sum(low$Night_Obs)/sum(low$Total_Obs)
high_noct <- sum(high$Night_Obs)/sum(high$Total_Obs)

successes_low <- low$Night_Obs
total_low <- low$Total_Obs

successes_high <- high$Night_Obs
total_high <- high$Total_Obs

# # lets test for a difference between these
# result <- prop.test(x = c(successes_low, successes_high), n = c(total_low, total_high), conf.level = 0.95)
# print(result)
# # it said chi squared approximation may be incorrect


n_successes_low <- sum(successes_low)
n_total_low <- sum(total_low)
n_failures_low <- sum(total_low) - sum(successes_low)

n_successes_high <- sum(successes_high)
n_total_high <- sum(total_high)
n_failures_high <- sum(total_high) - sum(successes_high)

table <- matrix(c(n_successes_low, n_failures_low, n_successes_high, n_failures_high), nrow = 2, byrow = TRUE)
colnames(table) <- c("Success", "Failure")
rownames(table) <- c("Group 1", "Group 2")
result <- fisher.test(table)
print(result)


