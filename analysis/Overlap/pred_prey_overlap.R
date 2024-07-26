# pred prey overlap for loop for all combinations
# margaret mercer
# july 26, 2024


# clear workspace
rm(list=ls())

library(tidyverse)
library(overlap)
library(raster)
library(sf)
library(terra)

data <- read_csv("../data_too_big/five_year_observation_data.csv")

data <- data %>%
  mutate(
    Local_Date = as.Date(Local_Date_Time),       # Extract date
    Local_Time = format(Local_Date_Time, "%H:%M:%S")  # Extract time
  )


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
  Overlap_Low = rep(NA, 18),
  Overlap_High = rep(NA, 18),
  Difference = rep(NA, 18),
  p_value = rep(NA, 18),
  Median_Human_Dist = rep(NA, 18),
  n_Predator_Low = rep(NA, 18),
  n_Predator_High = rep(NA, 18),
  n_Prey_Low = rep(NA, 18),
  n_Prey_High = rep(NA, 18),
  stringsAsFactors = FALSE
)


# for loop time!!!
# FIRST we loop through predators
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
        pred_name == "Canis lupus" & prey_name == "Mephitis mephitis") { # for some reason, the last two mesocarnivores (red fox and skunk)
                            # don't like the original wolf shape file
                            # so I had to make a new wolf shape file
                            # and for rox and skunk only, use that one
                            
      
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
      
      # # double check that this works by visualizing on map
      # spatial_inside <- st_make_valid(st_as_sf(df_inside, coords = c("Longitude", "Latitude"), crs = st_crs(prey_range)))
      # ggplot() +
      #   geom_sf(data = prey_range, size = 1.5, color = "black", fill = "#0075C4", alpha = 0.5) +
      #   geom_sf(data = new_wolf, size = 1.5, color = "black", fill = "#CB429F", alpha = 0.5) +
      #   geom_sf(data = range_overlap[1], size = 1.5, color = "black", fill = "#690375", alpha = 0.5) +
      #   ggtitle("Predator/Prey Overlap") +
      #   geom_sf(data = spatial_inside) +
      #   coord_sf()
      
      
    } else {
            
      
      # st_is_valid(prey_range, reason=TRUE)
      # st_is_valid(pred_range, reason=TRUE)
      
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
      
      # # double check that this works by visualizing on map
      # spatial_inside <- st_make_valid(st_as_sf(df_inside, coords = c("Longitude", "Latitude"), crs = st_crs(prey_range)))
      # ggplot() +
      #   geom_sf(data = prey_range, size = 1.5, color = "black", fill = "#0075C4", alpha = 0.5) +
      #   geom_sf(data = pred_range, size = 1.5, color = "black", fill = "#CB429F", alpha = 0.5) +
      #   geom_sf(data = range_overlap[1,], size = 1.5, color = "black", fill = "#690375", alpha = 0.5) +
      #   ggtitle("Predator/Prey Overlap") +
      #   geom_sf(data = spatial_inside) +
      #   coord_sf()
      
    }

    
    # # overlap analysis
    
    # subset to the two species
    
    pair <- filter(df_inside, Species_Name == pred_name | Species_Name == prey_name) 
    
    # find median and assign to an object
    
    median <- median(pair$Humans_Per_Camera_Per_Day)
    
    # filter to low human disturbance
    low_dist <- filter(pair, Humans_Per_Camera_Per_Day < median)
    
    # convert time into radians
    time <- as.POSIXct(low_dist$Local_Time, format = "%H:%M:%S")
    
    # Extract hours, minutes, and seconds
    hours <- as.numeric(format(time, "%H"))
    minutes <- as.numeric(format(time, "%M"))
    seconds <- as.numeric(format(time, "%S"))
    
    # Convert time to radians
    time_radians <- 2 * pi * ((hours + minutes / 60 + seconds / 3600) / 24)
    
    # get overlap for pred and prey for low disturbance
    low_pred <- time_radians[low_dist$Species_Name == pred_name]
    low_prey <- time_radians[low_dist$Species_Name == prey_name]
    # 
    # overlapPlot(low_pred, low_prey)
    # legend('topright', c("Predator", "Prey"), lty=c(1,2), col=c(1,4), bty='n')
    # 
    overlap_low <- overlapEst(low_pred, low_prey, type="Dhat4")
    overlap_low
    
    # calculate bootstrap values
    bootstrap_low <- bootstrap(low_pred, low_prey, 200, smooth=TRUE, kmax=3, adjust=NA, n.grid=128, type="Dhat4", cores=1)
    # now calculate confidence intervals
    bootCI(overlap_low, bootstrap_low, conf = 0.95)
    
    ## plot pred and prey overlap for HIGH disturbance
    
    high_dist <- filter(pair, Humans_Per_Camera_Per_Day >= median)
    
    # convert time into radians
    time <- as.POSIXct(high_dist$Local_Time, format = "%H:%M:%S")
    
    
    # Extract hours, minutes, and seconds
    hours <- as.numeric(format(time, "%H"))
    minutes <- as.numeric(format(time, "%M"))
    seconds <- as.numeric(format(time, "%S"))
    
    # Convert time to radians
    time_radians <- 2 * pi * ((hours + minutes / 60 + seconds / 3600) / 24)
    
    # plot pred and prey for high disturbance
    high_pred <- time_radians[high_dist$Species_Name == pred_name]
    high_prey <- time_radians[high_dist$Species_Name == prey_name]
    # 
    # overlapPlot(high_pred, high_prey)
    # legend('topright', c("Predator", "Prey"), lty=c(1,2), col=c(1,4), bty='n')
    # 
    overlap_high <- overlapEst(high_pred, high_prey, type="Dhat4")
    overlap_high
    
    # calculate bootstrap values
    bootstrap_high <- bootstrap(high_pred, high_prey, 200, smooth=TRUE, kmax=3, adjust=NA, n.grid=128, type="Dhat4", cores=1)
    # now calculate confidence intervals
    bootCI(overlap_high, bootstrap_high, conf = 0.95)
    
    
    t_test <- t.test(bootstrap_low, bootstrap_high, var.equal = TRUE)
    
    # set row number for adding results to results dataframe
    if (i == 1) {
      k <- j
    } else {
      k <- j + ((i-1)*9)
    }
    
    # print results to dataframe
    results[k, 1] <- pred_name
    results[k, 2] <- prey_name
    results[k, 3] <- overlap_low
    results[k, 4] <- overlap_high
    results[k, 5] <- (overlap_high - overlap_low)
    results[k, 6] <- t_test$p.value
    results[k, 7] <- median
    results[k, 8] <- length(low_pred)
    results[k, 9] <- length(high_pred)
    results[k, 10] <- length(low_prey)
    results[k, 11] <- length(high_prey)
    
  }
  
}


# add columns to results dataframe
results$Prey_Type <- c("herbivore",
                  "herbivore",
                  "herbivore",
                  "herbivore",
                  "mesocarnivore",
                  "mesocarnivore",
                  "mesocarnivore",
                  "mesocarnivore",
                  "mesocarnivore",
                  "herbivore",
                  "herbivore",
                  "herbivore",
                  "herbivore",
                  "mesocarnivore",
                  "mesocarnivore",
                  "mesocarnivore",
                  "mesocarnivore",
                  "mesocarnivore")

results <- results %>%
  mutate(Trend = if_else(Difference < 0, "decreasing", 
                         if_else(Difference > 0, "increasing", "no change")))

write.csv(results, "results/pred_prey_overlap_results.csv")
