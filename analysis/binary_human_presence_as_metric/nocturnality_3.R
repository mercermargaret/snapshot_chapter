# analysis of nocturnality for all species
# Margaret Mercer
# july 25, 2024


# clear workspace
rm(list=ls())


library(tidyverse)
library(lme4)

# replace with your file path
data <- read_csv("../data_too_big/five_year_observation_data.csv")

data <- data %>%
  mutate(
    Local_Date = as.Date(Local_Date_Time),       # Extract date
    Local_Time = format(Local_Date_Time, "%H:%M:%S")  # Extract time
  )

# filter out NAs for species because we don't care about them
data <- data %>%
  filter(!is.na(Species_Name))

# set species list
species_list <- c("Puma concolor", 
                  "Canis lupus", 
                  "Odocoileus virginianus",
                  "Odocoileus hemionus",
                  "Cervus canadensis",
                  "Alces alces",
                  "Canis latrans",
                  "Lynx rufus",
                  "Procyon lotor",
                  "Vulpes vulpes",
                  "Mephitis mephitis",
                  "Homo sapiens")

results <- data.frame(
  Species = rep(NA, 12),
  Noct_Overall = rep(NA, 12),
  Noct_Low = rep(NA, 12),
  Noct_High = rep(NA, 12),
  Noct_Diff = rep(NA, 12),
  "p-value" = rep(NA, 12),
  n_Photos = rep(NA, 12),
  n_Cameras = rep(NA, 12),
  n_Cameras_Low = rep(NA, 12),
  n_Cameras_High = rep(NA, 12),
  stringsAsFactors = FALSE
)


for (i in 1:length(species_list)) {
  
    # pick a species
    species_name <- species_list[i]
    
    # have it tell us what species its working on
    cat("Starting species: ",species_name)
    
  
    species <- subset(data, Species_Name == species_name)
    
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
    
    # split it into low/high and see what proportion of occurrences are at night between the two?
    overall_noct <- sum(props_updated$Night_Obs)/sum(props_updated$Total_Obs)
    
    low <- filter(props_updated, Humans_Per_Camera_Per_Day == 0)
    high <- filter(props_updated, Humans_Per_Camera_Per_Day > 0)
    
    low_noct <- sum(low$Night_Obs)/sum(low$Total_Obs)
    high_noct <- sum(high$Night_Obs)/sum(high$Total_Obs)
    
    successes_low <- low$Night_Obs
    total_low <- low$Total_Obs
    
    successes_high <- high$Night_Obs
    total_high <- high$Total_Obs
    
    n_successes_low <- sum(successes_low)
    n_total_low <- sum(total_low)
    n_failures_low <- sum(total_low) - sum(successes_low)
    
    n_successes_high <- sum(successes_high)
    n_total_high <- sum(total_high)
    n_failures_high <- sum(total_high) - sum(successes_high)
    
    table <- matrix(c(n_successes_low, n_failures_low, n_successes_high, n_failures_high), nrow = 2, byrow = TRUE)
    colnames(table) <- c("Success", "Failure")
    rownames(table) <- c("Group 1", "Group 2")
    fisher_test <- fisher.test(table)
    
    # print results to dataframe
    results[i, 1] <- species_name
    results[i, 2] <- overall_noct
    results[i, 3] <- low_noct
    results[i, 4] <- high_noct
    results[i, 5] <- (high_noct - low_noct)
    results[i, 6] <- fisher_test$p.value
    results[i, 7] <- length(species$Site_Name)
    results[i, 8] <- length(props$Site_Name)
    results[i, 9] <- length(low$Site_Name)
    results[i, 10] <- length(high$Site_Name)

}




results$Type <- c("carnivore",
                  "carnivore",
                  "herbivore",
                  "herbivore",
                  "herbivore",
                  "herbivore",
                  "mesocarnivore",
                  "mesocarnivore",
                  "mesocarnivore",
                  "mesocarnivore",
                  "mesocarnivore",
                  "human")

results <- results %>%
  mutate(Trend = if_else(Noct_Diff < 0, "decreasing", 
                         if_else(Noct_Diff > 0, "increasing", "no change")))


write.csv(results, "results/nocturnality_3.csv")


