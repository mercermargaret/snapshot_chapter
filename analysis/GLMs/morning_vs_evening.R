# morning vs evening activity as human presence increases
# margaret mercer
# july 18, 2024
# (visualize this in "Nocturnality_Visualization")


library(tidyverse)
library(lme4)

data <- read.csv("../data_too_big/all_years.csv")

species <- subset(data, Species_Name == 'Puma concolor' |
                  Species_Name == 'Canis lupus' |
                  Species_Name == 'Odocoileus virginianus' |
                  Species_Name == 'Odocoileus hemionus' |
                  Species_Name == 'Cervus canadensis' |
                  Species_Name == 'Alces alces' |
                  Species_Name == 'Canis latrans' |
                  Species_Name == 'Lynx rufus' |
                  Species_Name == 'Procyon lotor' |
                  Species_Name == 'Vulpes vulpes' |
                  Species_Name == 'Mephitis mephitis')

# add column for morning or evening
species$Before_Noon <- ifelse(species$Local_Time < 12, 1, 0)


binomial <- species %>%
  group_by(Site_Name) %>%
  summarize(Prop_Morn = mean(Before_Noon)) # Calculate binomial proportions for each site
counts <- species %>%
  group_by(Site_Name) %>%
  summarize(Morn_Obs = sum(Before_Noon == 1),
            Total_Obs = n()) # Create columns for #morningobservations/site and total#observations/site.
props <- merge(counts, binomial, by = "Site_Name") # merge together. now we have a dataframe with each site, the total morning observations, total observations, and the proportion of the two.


# now I want to add "humans per camera per day" column to that (I still want only one row per camera)
unique_species <- species %>% distinct(Site_Name, .keep_all = TRUE)
unique_species <- unique_species %>% dplyr::select(Site_Name, Humans_Per_Camera_Per_Day)
props_updated <- props %>% 
  left_join(unique_species, by = "Site_Name")

# what if I split it into low/high and see what proportion of occurrences are in the morning between the two?
overall_morn <- sum(props_updated$Morn_Obs)/sum(props_updated$Total_Obs)

low <- filter(props_updated, Humans_Per_Camera_Per_Day < median(props_updated$Humans_Per_Camera_Per_Day))
high <- filter(props_updated, Humans_Per_Camera_Per_Day >= median(props_updated$Humans_Per_Camera_Per_Day))

low_morn <- sum(low$Morn_Obs)/sum(low$Total_Obs)
high_morn <- sum(high$Morn_Obs)/sum(high$Total_Obs)

successes_low <- low$Morn_Obs
total_low <- low$Total_Obs

successes_high <- high$Morn_Obs
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

# summarize all results
morn <- read_csv("data/Morning_vs_Evening_Results.csv")

minus_humans <- morn[1:11, ]

summary(minus_humans$Morn_Overall)
