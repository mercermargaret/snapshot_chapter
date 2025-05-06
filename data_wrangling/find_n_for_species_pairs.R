# find n for species pairs
# margaret mercer
# may 5, 2025

# clear workspace
rm(list=ls())

library(tidyverse)
library(overlap)
library(raster)
library(sf)
library(terra)

data <- read_csv("../data_too_big/five_year_observation_data.csv")

# filter out NAs for species because we don't care about them
data <- data %>%
  filter(!is.na(Species_Name))

# set species lists
pred_list <- c("Puma concolor", 
               "Canis lupus")

prey_list <- c("Odocoileus virginianus",
               "Odocoileus hemionus",
               "Cervus canadensis",
               "Alces alces",
               "Canis latrans",
               "Lynx rufus",
               "Procyon lotor",
               "Vulpes vulpes",
               "Mephitis mephitis")

puma <- st_read('data/subset_shape_files/Puma')
wolf <- st_read('data/subset_shape_files/Wolf')
whitetail <- st_read('data/subset_shape_files/Whitetail')
mule <- st_read('data/subset_shape_files/Mule Deer')
elk <- st_read('data/subset_shape_files/Elk')
moose <- st_read('data/subset_shape_files/Moose')
coyote <- st_read('data/subset_shape_files/Coyote')
bobcat <- st_read('data/subset_shape_files/Bobcat')
raccoon <- st_read('data/subset_shape_files/Raccoon')
fox <- st_read('data/subset_shape_files/Red Fox')
skunk <- st_read('data/subset_shape_files/Striped Skunk')

pred_range_list <- list(puma,
                        wolf) 

prey_range_list <- list(whitetail,
                        mule,
                        elk,
                        moose,
                        coyote,
                        bobcat,
                        raccoon,
                        fox,
                        skunk)

results <- data.frame(
  Predator = rep(NA, 18),
  Prey = rep(NA, 18),
  Predator_n_Deps = rep(NA, 18),
  Predator_n_Obs = rep(NA, 18),
  Prey_n_Deps = rep(NA, 18),
  Prey_n_Obs = rep(NA, 18)
)


for (i in 1:length(pred_list)) {
  pred_name <- pred_list[i] # select pred from list
  
  for (j in 1:length(prey_list)) {
    
    prey_name <- prey_list[j] # select prey from list
    cat("Starting: ", pred_name, "/",prey_name)
    
    # get range data
    pred_range <- pred_range_list[[i]]
    prey_range <- prey_range_list[[j]]
    
    # Convert df to an sf object
    points_sf <- st_make_valid(st_as_sf(data, coords = c("Longitude", "Latitude"), crs = st_crs(prey_range)))
    
    if (pred_name == "Canis lupus" & prey_name == "Vulpes vulpes" |
        pred_name == "Canis lupus" & prey_name == "Mephitis mephitis") { 
      # for some reason, the last two mesocarnivores (red fox and skunk)
      # don't like the original wolf shape file
      # so I had to make a new wolf shape file
      # and for fox and skunk only, use that one
      
      
      st_is_valid(prey_range, reason=TRUE)
      new_wolf <- st_read('data/subset_shape_files/New_Wolf')
      st_is_valid(new_wolf, reason=TRUE)
      sf_use_s2(FALSE)
      range_overlap <- st_intersection(prey_range, new_wolf)
      st_is_valid(range_overlap, reason=TRUE)
      range_overlap <- st_transform(range_overlap, crs=9822) # transform object to projected coordinates
      st_crs(range_overlap)$units # check units - should be meters
      range_overlap <- st_buffer(range_overlap, 1) # if meters, buffer by 1 m
      range_overlap <- st_transform(range_overlap, crs=4326) # transform back to original CRS
      range_overlap <- st_union(range_overlap) # merge again 
      s2::s2_rebuild(range_overlap)
      sf_use_s2(TRUE)
      range_overlap <- st_make_valid(range_overlap)
      inside <- st_within(points_sf, range_overlap, sparse = FALSE)
      df_inside <- data[which(inside[,1]),]

      
    } else {
      

      
      # switch off spherical geometry
      sf_use_s2(FALSE)
      
      # now calculate overlap
      range_overlap <- st_intersection(prey_range, pred_range)
      st_is_valid(range_overlap, reason=TRUE)
      
      sf_use_s2(TRUE) # when I leave this off, 
      
      # Perform the point-in-polygon test
      inside <- st_within(points_sf, range_overlap, sparse = FALSE)
      
      # Extract rows from df that are inside the polygon
      df_inside <- data[which(inside[,1]),]
      
      # df_inside now contains only the rows where coordinates fall inside the polygon

    }
    

    # subset to each species
    prey_obs <- filter(df_inside, Species_Name == prey_name) 
    pred_obs <- filter(df_inside, Species_Name == pred_name) 
    
    # get number of observations of each species
    prey_n_obs <- length(prey_obs$Local_Date_Time)
    pred_n_obs <- length(pred_obs$Local_Date_Time)
    
    # subset to all cameras within species range where at least one of a species was found
    prey_deps <- prey_obs %>%
      group_by(Array) %>%
      slice(1) %>%
      ungroup()
    
    pred_deps <- pred_obs %>%
      group_by(Array) %>%
      slice(1) %>%
      ungroup()
    
    # get number of deployments of each species
    prey_n_deps <- length(prey_deps$Array)
    pred_n_deps <- length(pred_deps$Array)
    
    # set row number for adding results to results dataframe
    if (i == 1) {
      k <- j
    } else {
      k <- j + ((i-1)*9)
    }
    
    # print results to dataframe
    results[k, 1] <- pred_name
    results[k, 2] <- prey_name
    results[k, 3] <- pred_n_deps
    results[k, 4] <- pred_n_obs
    results[k, 5] <- prey_n_deps
    results[k, 6] <- prey_n_obs
    
  }
}



write.csv(results, "results/pred_prey_n.csv", row.names = FALSE) 
    

    
    
    
    

    