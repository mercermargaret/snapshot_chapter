# rounding results for tables
# margaret mercer
# march 28 2025

# load packages
library(tidyverse)

# load data
temp_overlap <- read.csv("results/pred_prey_overlap_results.csv")
temp_individual <- read.csv("results/nocturnality.csv")
temp_puma_vs_wolf <- read.csv("results/puma_vs_wolf_temporal_overlap.csv")
spat_overlap <- read.csv("results/pred_prey_occupancy_with_detection_results.csv")
spat_individual <- read.csv("results/single_species_occupancy_results.csv")
spat_puma_vs_wolf <- read.csv("results/puma_vs_wolf_spatial_overlap.csv")

# round
temp_overlap_rounded <- temp_overlap %>%
  mutate(
    across(where(is.numeric) & !starts_with("p_value"), ~ round(. , digits = 2)), # round
    p_value = ifelse(p_value < 0.001, "< 0.001", as.character(round(p_value, digits = 3))) # modify p value column
  )
temp_overlap_rounded <- temp_overlap_rounded[c(2:12)]

temp_individual_rounded <- temp_individual %>%
  mutate(
    across(where(is.numeric) & !starts_with("p.value"), ~ round(. , digits = 2)), # round
    p.value = ifelse(p.value < 0.001, "< 0.001", as.character(round(p.value, digits = 3))) # modify p value column
  )
temp_individual_rounded <- temp_individual_rounded[c(1, 3:8)]

temp_puma_vs_wolf_rounded <- temp_puma_vs_wolf %>%
  mutate(
    across(where(is.numeric), ~ round(. , digits = 2)), # round
  )
temp_puma_vs_wolf_rounded <- temp_puma_vs_wolf_rounded[1:3] # get rid of columns I don't want
temp_puma_vs_wolf_rounded <- pivot_wider(temp_puma_vs_wolf_rounded, 
                                         names_from = Predator, 
                                         values_from = Overlap) # make wide
temp_puma_vs_wolf_rounded <- rename(temp_puma_vs_wolf_rounded, 
                                    c("Overlap with Pumas" = "Puma concolor", 
                                      "Overlap with Wolves" = "Canis lupus"))


spat_overlap_rounded <- spat_overlap %>%
  mutate(
    across(where(is.numeric), ~ round(. , digits = 2)), # round
  )
spat_overlap_rounded <- spat_overlap_rounded[c(1:3, 5, 11, 13)] # get rid of unnecessary columns


spat_individual_rounded <- spat_individual %>%
  mutate(
    across(where(is.numeric), ~ round(. , digits = 2)), # round
  )
spat_individual_rounded <- spat_individual_rounded[c(1:2, 4, 10, 11)] # get rid of unnecessary columns

spat_puma_vs_wolf_rounded <- spat_puma_vs_wolf %>%
  mutate(
    across(where(is.numeric), ~ round(. , digits = 2)), # round
  )
spat_puma_vs_wolf_rounded <- spat_puma_vs_wolf_rounded[1:3] # get rid of columns I don't want
spat_puma_vs_wolf_rounded <- pivot_wider(spat_puma_vs_wolf_rounded, 
                                         names_from = Predator, 
                                         values_from = overlap) # make wide
spat_puma_vs_wolf_rounded <- rename(spat_puma_vs_wolf_rounded, 
                                    c("Overlap with Pumas" = "Puma concolor", 
                                      "Overlap with Wolves" = "Canis lupus"))

# replace all scientific names with common names
# create reference table
renaming <- data.frame(
  species = c("Puma concolor", 
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
              "Homo sapiens"),  # Species names
  common_name = c("Puma",
                  "Wolf",
                  "White-tailed Deer",
                  "Mule Deer",
                  "Elk",
                  "Moose",
                  "Coyote",
                  "Bobcat",
                  "Raccoon",
                  "Red Fox",
                  "Striped Skunk",
                  "Human")  # Corresponding common names
)

# create a function to replace common with species names
replace_with_common_names <- function(df, renaming) {
  # Loop through all columns in the dataframe
  df[] <- lapply(df, function(col) {
    # Only replace species names in columns that are character or factor types
    if (is.character(col) | is.factor(col)) {
      # Replace species names with common names if they exist in the mapping table
      col <- sapply(col, function(x) {
        if (x %in% renaming$species) {
          # Replace with common name if species matches
          return(renaming$common_name[renaming$species == x])
        } else {
          # Leave the value unchanged if it's not a species name
          return(x)
        }
      })
    }
    return(col)
  })
  return(df)
}

# make list of dataframes
list <- list(temp_overlap_rounded = temp_overlap_rounded, 
             temp_individual_rounded = temp_individual_rounded, 
             temp_puma_vs_wolf_rounded = temp_puma_vs_wolf_rounded,
             spat_overlap_rounded = spat_overlap_rounded,
             spat_individual_rounded = spat_individual_rounded,
             spat_puma_vs_wolf_rounded = spat_puma_vs_wolf_rounded)

# apply  function to each dataframe in the list
list <- lapply(list, replace_with_common_names, renaming)

# print
write.csv(list$temp_overlap_rounded, "results/Tables/temp_overlap.csv", row.names = FALSE)
write.csv(list$temp_individual_rounded, "results/Tables/temp_individual.csv", row.names = FALSE)
write.csv(list$temp_puma_vs_wolf_rounded, "results/Tables/temp_overlap_total.csv", row.names = FALSE)
write.csv(list$spat_overlap_rounded, "results/Tables/spat_overlap.csv", row.names = FALSE)
write.csv(list$spat_individual_rounded, "results/Tables/spat_individual.csv", row.names = FALSE)
write.csv(list$spat_puma_vs_wolf_rounded, "results/Tables/spat_overlap_total.csv", row.names = FALSE)
