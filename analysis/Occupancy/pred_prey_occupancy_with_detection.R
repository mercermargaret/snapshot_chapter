# occupancy file to loop through pred AND prey, low AND high humans
# oh boi lets try this out
# margaret mercer
# august 16, 2024

# note: this file takes around 20 min to run

# load packages
library(lubridate)
library(sf)
library(tidyverse)
library(unmarked)
library(AICcmodavg)
library(ggplot2)
library(TMB)

# clear workspace
rm(list=ls())

# import and wrangle data ####
# let's see if we can merge these in a way that keeps ALL site names and just has a row of "NA"s if there were no pics.
deployments_all <- read.csv("data/five_year_deployments.csv") 
deployments_all$Array_Year <- paste(deployments_all$Array, deployments_all$Year, sep = "_")
observations_all <- read.csv("../data_too_big/five_year_observation_data.csv") 
joined <- left_join(deployments_all, observations_all, by = "Site_Name")
joined$Latitude <- joined$Latitude.x
joined$Longitude <- joined$Longitude.x
joined$Survey_Nights <- joined$Survey_Nights.x
joined$Habitat <- joined$Habitat.x
joined$Development_Level <- joined$Development_Level.x
joined$Disturbance <- joined$Disturbance.x
joined$Humans_Per_Camera_Per_Day <- joined$Humans_Per_Camera_Per_Day.x
observations_all <- subset(joined, select = c("record_ID", 
                                              "Array_Year", 
                                              "Site_Name", 
                                              "Survey_Nights", 
                                              "Latitude", 
                                              "Longitude", 
                                              "Local_Date_Time", 
                                              "Species_Name", 
                                              "Time_Zone",
                                              "UTC_Date_Time",
                                              "Year",
                                              "Start_Date",
                                              "End_Date",
                                              "Habitat",
                                              "Development_Level",
                                              "Disturbance",
                                              "Humans_Per_Camera_Per_Day")) 

# turn NAs in human column to 0 (NAs are there because there were no humans!)
observations_all$Humans_Per_Camera_Per_Day <- ifelse(is.na(observations_all$Humans_Per_Camera_Per_Day), 0, 
                                                     observations_all$Humans_Per_Camera_Per_Day)
deployments_all$Humans_Per_Camera_Per_Day <- ifelse(is.na(deployments_all$Humans_Per_Camera_Per_Day), 0, 
                                                    deployments_all$Humans_Per_Camera_Per_Day)

# set up the for loop! ####

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

# create empty dataframe for occupancy values
results_occ <- data.frame(
  Predator = character(18), 
  Prey = character(18),
  "overlap_low" = numeric(18), 
  "p-value_low" = numeric(18), 
  "overlap_high" = numeric(18), 
  "p-value_high" = numeric(18),
  "lower_CI_low" = numeric(18),
  "upper_CI_low" = numeric(18),
  "lower_CI_high" = numeric(18),
  "upper_CI_high" = numeric(18),
  stringsAsFactors = FALSE
)

# and one for detection values
results_det <- results_occ

# begin the for loop ####
# since i and j are already used within this, we'll use k and l

for (k in 1:length(pred_list)) {
  pred_name <- pred_list[k] # select pred from list
  
  for (l in 1:length(prey_list)) {
    
    prey_name <- prey_list[l] # select prey from list
    cat("Starting: ", pred_name, "/",prey_name)
    
    # geographic subsetting of "observations_all" and "deployments_all" to only the species pair overlapping range
    pred_range <- pred_range_list[[k]]
    prey_range <- prey_range_list[[l]]
    
    # Convert df to an sf object
    points_sf_obs <- st_make_valid(st_as_sf(observations_all, 
                                            coords = c("Longitude", "Latitude"), 
                                            crs = st_crs(prey_range)))
    
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
      inside_obs <- st_within(points_sf_obs, range_overlap, sparse = FALSE)
      obs_within_range <- observations_all[which(inside_obs[,1]),]
      
      # trim deployments too
      
      sf_use_s2(FALSE)
      points_sf_deps <- st_make_valid(st_as_sf(deployments_all, 
                                               coords = c("Longitude", "Latitude"), 
                                               crs = st_crs(range_overlap)))
      st_is_valid(points_sf_deps, reason=TRUE)
      sf_use_s2(TRUE)
      inside_deps <- st_within(points_sf_deps, range_overlap, sparse = FALSE)
      deps_within_range <- deployments_all[which(inside_deps[,1]),]
      
      
    } else {
      
      # switch off spherical geometry
      sf_use_s2(FALSE)
      
      # now calculate overlap
      range_overlap <- st_intersection(prey_range, pred_range)
      st_is_valid(range_overlap, reason=TRUE)
      
      sf_use_s2(TRUE)
      inside_obs <- st_within(points_sf_obs, range_overlap, sparse = FALSE)
      obs_within_range <- observations_all[which(inside_obs[,1]),]
      
      # trim deployments too
      
      sf_use_s2(FALSE)
      points_sf_deps <- st_make_valid(st_as_sf(deployments_all, 
                                               coords = c("Longitude", "Latitude"), 
                                               crs = st_crs(range_overlap)))
      st_is_valid(points_sf_deps, reason=TRUE)
      sf_use_s2(TRUE)
      inside_deps <- st_within(points_sf_deps, range_overlap, sparse = FALSE)
      deps_within_range <- deployments_all[which(inside_deps[,1]),]
      
    }
    
    
    # split down median of SITES, not observations
    # find median dist and assign to an object
    median <- median(deps_within_range$Humans_Per_Camera_Per_Day)
    
    
    # predator occupancy low ####
    # only include deployment AND observation data from LOWER half of human activity!
    # filter to low human disturbance for observations and sites
    observations_low <- filter(obs_within_range, Humans_Per_Camera_Per_Day < median)
    site_info_low <- filter(deps_within_range, Humans_Per_Camera_Per_Day < median)
    
    # CREATE ENCOUNTER HISTORY FOR LOW PREDATOR
    n_sites <- length(unique(observations_low$Site_Name))
    sampling_int <- 7
    max_events <- ceiling((max(observations_low$Survey_Nights))/sampling_int)
    hist_low_pred <- matrix(NA, ncol = max_events, nrow = n_sites)
    colnames(hist_low_pred) <- paste0("V",seq(1,max_events))
    
    # Add a column for site name
    Site <- c(unique(observations_low$Site_Name))
    hist_low_pred <- cbind(hist_low_pred, Site)
    
    # Copy empty dataframe so we can fill it with other covariates
    
    hist_low_pred <- as.data.frame(hist_low_pred)
    DOY_low_pred <- hist_low_pred
    survey_days_low_pred <- hist_low_pred
    
    # this took ~ 5 min
    for(i in 1:length(unique(observations_low$Site_Name))){
      
      site_i <- unique(observations_low$Site_Name) [i]
      cat("Starting site ",site_i,"\n") # tells you how far along you are
      data_i <- observations_low[which(observations_low$Site_Name == site_i & 
                                         observations_low$Species_Name == pred_name), ]
      
      start_i <- site_info_low$Start_Date[which(site_info_low$Site==site_i)]
      end_i <- site_info_low$End_Date[which(site_info_low$Site==site_i)]
      days_i <- site_info_low$Survey_Nights[which(site_info_low$Site==site_i)]
      
      events_i <- as.numeric(ceiling(days_i/sampling_int))
      # I used ceiling() because if you have 4.3 sampling
      # events you'll want to consider that as 5 sampling events, 4 complete
      # sampling events and one partial event. 
      
      # Now loop through these sampling events and pull out any detections
      # that occur within a given sampling event.
      
      # Do the first event outside the loop so that you can use start_i as the start of the first event.
      start_1 <- start_i
      end_1 <- as.Date(start_1) + sampling_int - 1
      
      # Ask if your camera data has any detections within this interval.
      data_1 <- which(as.Date(data_i$Local_Date) >= start_1 & as.Date(data_i$Local_Date) <= end_1) 
      
      # Is there at least one detection?
      if(length(data_1) > 0) {  # If so, fill in hist_low_pred 
        
        # Put the data in the first column since that will always represent your first detection
        hist_low_pred[which(hist_low_pred$Site==site_i),paste0("V",1)] <- 1 # Detected
        
        # NOTE: You could modify this code to put the actual number of detections
        # into EN, instead of just 1 or 0, and then convert to 0/1 later. That might
        # be useful if you are interested in the number of detections. 
        
      } else { # If not, fill in your hist_low_pred with a zero.=
        hist_low_pred[which(hist_low_pred$Site==site_i),paste0("V",1)] <- 0 # Not detected
        
      }
      
      
      if(end_1 > end_i){
        s_days_1 <- as.Date(end_i) - as.Date(start_1) + 1
      } else {
        s_days_1 <- as.Date(end_1) - as.Date(start_1) + 1
      } # get number of actual survey days
      
      survey_days_low_pred[which(hist_low_pred$Site==site_i),paste0("V",1)] <- s_days_1 
      # add number of survey days to surveys df
      DOY_low_pred[which(hist_low_pred$Site==site_i),paste0("V",1)] <- yday(as.Date(start_1) + (as.numeric(s_days_1)/2)) 
      # calculate day of year and add to DOY_low_pred df
      
      
      
      if(days_i > 7) { 
        # we need to do this to tell it to ONLY loop through other sampling events 
        # if there is more than one samping event (if days_i is greater than 7)
        
        
        # Now start looping through the other sampling events
        for(j in 2:events_i){
          
          start_j <- as.Date(start_1) + ((j - 1) * sampling_int)
          # Compare this start_j against end_1 to be sure that the j-th event starts as soon as the j-1th event ends
          end_j <- as.Date(start_j) + sampling_int - 1
          
          data_j <- which(as.Date(data_i$Local_Date) >= as.Date(start_j) & 
                            as.Date(data_i$Local_Date) <= as.Date(end_j))
          
          # the following is the same as above:
          if(length(data_j) > 0){
            hist_low_pred[which(hist_low_pred$Site==site_i),paste0("V",j)] <- 1 # Detected
          } else {
            hist_low_pred[which(hist_low_pred$Site==site_i),paste0("V",j)] <- 0 # Not detected
          }
          
          if(end_j > end_i){
            s_days_j <- as.Date(end_i) - as.Date(start_j) + 1
          } else {
            s_days_j <- as.Date(end_j) - as.Date(start_j) + 1
          }
          
          survey_days_low_pred[which(hist_low_pred$Site==site_i),paste0("V",j)] <- s_days_j
          DOY_low_pred[which(hist_low_pred$Site==site_i),paste0("V",j)] <- yday(as.Date(start_j) + (as.numeric(s_days_j)/2))
          
        }
        
      } else { 
        
        next
        
      }
      
    }
    
    # make sure year is a character not a number!!
    site_info_low$Year <- as.character(site_info_low$Year)
    
    # OCCUPANCY ANALYSIS OF LOW PREDATOR
    # z score standardize covariates
    site_info_low$Humans_Per_Camera_Per_Day <- scale((site_info_low$Humans_Per_Camera_Per_Day))
    site_info_low$Disturbance <- scale(as.matrix(site_info_low$Disturbance))
    
    # observation level covariates we have to do manually since "scale" only scales column by column
    DOY_low_pred <- data.frame(lapply(DOY_low_pred, as.numeric))
    DOY_low_pred <- as.matrix(DOY_low_pred[,grep("V",colnames(DOY_low_pred))])
    mean_DOY <- mean(as.vector(DOY_low_pred), na.rm = T)
    sd_DOY <- sd(as.vector(DOY_low_pred), na.rm = T)
    DOY_scaled <- (DOY_low_pred - mean_DOY)/sd_DOY
    
    survey_days_low_pred <- data.frame(lapply(survey_days_low_pred, as.numeric))
    survey_days_low_pred <- as.matrix(survey_days_low_pred[,grep("V",colnames(survey_days_low_pred))])
    mean_days <- mean(as.vector((survey_days_low_pred)), na.rm = T)
    sd_days <- sd(as.vector((survey_days_low_pred)), na.rm = T)
    days_scaled <- (survey_days_low_pred - mean_days)/sd_days
    
    # pull out data
    hist_low_pred <- hist_low_pred[,grep("V",colnames(hist_low_pred))]
    hist_low_pred[] <- lapply(hist_low_pred, as.numeric)
    
    # start here
    site_covs <- as.data.frame(site_info_low[,c("Humans_Per_Camera_Per_Day", 
                                                "Disturbance", 
                                                "Array_Year", 
                                                "Year")]) 
    site_covs <- site_covs %>%
      rename(
        Humans = "Humans_Per_Camera_Per_Day",
        Disturbance = "Disturbance")
    
    
    obs_covs <- list(DOY_scaled = DOY_scaled,
                     days_scaled = days_scaled) 
    
    # create object with all our data and covariates
    umf <- unmarkedFrameOccu(y = hist_low_pred, # Encounter history, must be a data frame or matrix
                             siteCovs = site_covs, # Site covariates, must be a data frame
                             obsCovs = obs_covs) # Observer covariates, must be list of data frames or matrices
    
    pred_model_low <- occu(~ days_scaled ~ 1 + (1 | Array_Year), data = umf) 
    # what do here? which value do we pull out, if we're accounting for days scaled? How do we deal with random effects?
    
    summary(pred_model_low)
    
    # PULL OUT PREDATOR LOW OCCUPANCY ESTIMATES FOR USE IN LOW PREY
    # grab predator occupancy by site
    pred_site <- ranef(pred_model_low) # this should preserve the order of the sites
    # pred_site
    
    # pull out means
    means <- bup(pred_site, stat = "mean")
    head(means) # this should look like the first few "means" under "pred_site"
    length(means) # this should match the number of rows of all other covariates
    
    # add onto site-level covariates for use in prey
    site_info_low$Predator_Occupancy <- means
    
    # convert to dataframe
    # check what each column is
    print(sapply(site_info_low, class))
    
    # Convert matrix column
    if (is.matrix(site_info_low$Humans_Per_Camera_Per_Day)) {
      matrix_col_df <- as.data.frame(site_info_low$Humans_Per_Camera_Per_Day)
      colnames(matrix_col_df) <- paste("Humans_Per_Camera_Per_Day", 1:ncol(matrix_col_df), sep = "_")
      site_info_low <- cbind(site_info_low, matrix_col_df)
      site_info_low$Humans_Per_Camera_Per_Day <- NULL  # Remove the original matrix column
    }
    
    if (is.matrix(site_info_low$Disturbance)) {
      matrix_col_df <- as.data.frame(site_info_low$Disturbance)
      colnames(matrix_col_df) <- paste("Disturbance", 1:ncol(matrix_col_df), sep = "_")
      site_info_low <- cbind(site_info_low, matrix_col_df)
      site_info_low$Disturbance <- NULL  # Remove the original matrix column
    }
    
    # Check the class of each column again
    print(sapply(site_info_low, class))
    
    
    # prey occupancy low ####
    
    # CREATE ENCOUNTER HISTORY FOR LOW PREY
    n_sites <- length(unique(observations_low$Site_Name))
    sampling_int <- 7
    max_events <- ceiling((max(observations_low$Survey_Nights))/sampling_int)
    hist_low_prey <- matrix(NA, ncol = max_events, nrow = n_sites)
    colnames(hist_low_prey) <- paste0("V",seq(1,max_events))
    
    # Add a column for site name
    Site <- c(unique(observations_low$Site_Name))
    hist_low_prey <- cbind(hist_low_prey, Site)
    
    # Copy empty dataframe so we can fill it with other covariates
    
    hist_low_prey <- as.data.frame(hist_low_prey)
    DOY_low_prey <- hist_low_prey
    survey_days_low_prey <- hist_low_prey
    
    # this took ~ 5 min
    for(i in 1:length(unique(observations_low$Site_Name))){
      
      site_i <- unique(observations_low$Site_Name) [i]
      cat("Starting site ",site_i,"\n") # tells you how far along you are
      data_i <- observations_low[which(observations_low$Site_Name == site_i & 
                                         observations_low$Species_Name == prey_name), ]
      
      start_i <- site_info_low$Start_Date[which(site_info_low$Site==site_i)]
      end_i <- site_info_low$End_Date[which(site_info_low$Site==site_i)]
      days_i <- site_info_low$Survey_Nights[which(site_info_low$Site==site_i)]
      
      events_i <- as.numeric(ceiling(days_i/sampling_int))
      # I used ceiling() because if you have 4.3 sampling
      # events you'll want to consider that as 5 sampling events, 4 complete
      # sampling events and one partial event. 
      
      # Now loop through these sampling events and pull out any detections
      # that occur within a given sampling event.
      
      # Do the first event outside the loop so that you can use start_i as the start of the first event.
      start_1 <- start_i
      end_1 <- as.Date(start_1) + sampling_int - 1
      
      # Ask if your camera data has any detections within this interval.
      data_1 <- which(as.Date(data_i$Local_Date) >= start_1 & as.Date(data_i$Local_Date) <= end_1) 
      
      # Is there at least one detection?
      if(length(data_1) > 0) {  # If so, fill in hist_low_prey 
        
        # Put the data in the first column since that will always represent your first detection
        hist_low_prey[which(hist_low_prey$Site==site_i),paste0("V",1)] <- 1 # Detected
        
        # NOTE: You could modify this code to put the actual number of detections
        # into EN, instead of just 1 or 0, and then convert to 0/1 later. That might
        # be useful if you are interested in the number of detections. 
        
      } else { # If not, fill in your hist_low_prey with a zero.=
        hist_low_prey[which(hist_low_prey$Site==site_i),paste0("V",1)] <- 0 # Not detected
        
      }
      
      
      if(end_1 > end_i){
        s_days_1 <- as.Date(end_i) - as.Date(start_1) + 1
      } else {
        s_days_1 <- as.Date(end_1) - as.Date(start_1) + 1
      } # get number of actual survey days
      
      survey_days_low_prey[which(hist_low_prey$Site==site_i),paste0("V",1)] <- s_days_1 
      # add number of survey days to surveys df
      DOY_low_prey[which(hist_low_prey$Site==site_i),paste0("V",1)] <- yday(as.Date(start_1) + (as.numeric(s_days_1)/2)) 
      # calculate day of year and add to DOY_low_prey df
      
      
      
      if(days_i > 7) { 
        # we need to do this to tell it to ONLY loop through other sampling events 
        # if there is more than one samping event (if days_i is greater than 7)
        
        
        # Now start looping through the other sampling events
        for(j in 2:events_i){
          
          start_j <- as.Date(start_1) + ((j - 1) * sampling_int)
          # Compare this start_j against end_1 to be sure that the j-th event starts as soon as the j-1th event ends
          end_j <- as.Date(start_j) + sampling_int - 1
          
          data_j <- which(as.Date(data_i$Local_Date) >= as.Date(start_j) & 
                            as.Date(data_i$Local_Date) <= as.Date(end_j))
          
          # the following is the same as above:
          if(length(data_j) > 0){
            hist_low_prey[which(hist_low_prey$Site==site_i),paste0("V",j)] <- 1 # Detected
          } else {
            hist_low_prey[which(hist_low_prey$Site==site_i),paste0("V",j)] <- 0 # Not detected
          }
          
          if(end_j > end_i){
            s_days_j <- as.Date(end_i) - as.Date(start_j) + 1
          } else {
            s_days_j <- as.Date(end_j) - as.Date(start_j) + 1
          }
          
          survey_days_low_prey[which(hist_low_prey$Site==site_i),paste0("V",j)] <- s_days_j
          DOY_low_prey[which(hist_low_prey$Site==site_i),paste0("V",j)] <- yday(as.Date(start_j) + (as.numeric(s_days_j)/2))
          
        }
        
      } else { 
        
        next
        
      }
      
    }
    
    # make sure year is a character not a number!!
    site_info_low$Year <- as.character(site_info_low$Year)
    
    # OCCUPANCY ANALYSIS OF LOW PREY
    
    # load and format covariates
    # z score standardize covariates
    site_info_low$Humans_Per_Camera_Per_Day <- scale((site_info_low$Humans_Per_Camera_Per_Day))
    site_info_low$Disturbance <- scale(as.matrix(site_info_low$Disturbance))
    site_info_low$Predator_Occupancy <- scale(as.matrix(site_info_low$Predator_Occupancy))
    
    # observation level covariates we have to do manually since "scale" only scales column by column
    DOY_low_prey <- data.frame(lapply(DOY_low_prey, as.numeric))
    DOY_low_prey <- as.matrix(DOY_low_prey[,grep("V",colnames(DOY_low_prey))])
    mean_DOY <- mean(as.vector(DOY_low_prey), na.rm = T)
    sd_DOY <- sd(as.vector(DOY_low_prey), na.rm = T)
    DOY_scaled <- (DOY_low_prey - mean_DOY)/sd_DOY
    
    survey_days_low_prey <- data.frame(lapply(survey_days_low_prey, as.numeric))
    survey_days_low_prey <- as.matrix(survey_days_low_prey[,grep("V",colnames(survey_days_low_prey))])
    mean_days <- mean(as.vector((survey_days_low_prey)), na.rm = T)
    sd_days <- sd(as.vector((survey_days_low_prey)), na.rm = T)
    days_scaled <- (survey_days_low_prey - mean_days)/sd_days
    
    
    # pull out data
    hist_low_prey <- hist_low_prey[,grep("V",colnames(hist_low_prey))]
    hist_low_prey[] <- lapply(hist_low_prey, as.numeric)
    
    site_covs <- as.data.frame(site_info_low[,c("Humans_Per_Camera_Per_Day", 
                                                "Disturbance", 
                                                "Array_Year", 
                                                "Year",
                                                "Predator_Occupancy")]) 
    site_covs <- site_covs %>%
      rename(
        Humans = "Humans_Per_Camera_Per_Day",
        Disturbance = "Disturbance")
    
    # make a new dataframe out of predator occupancy with same dimensions as other observation covariates
    pred_occupancy <- data.frame(replicate(length(DOY_scaled[1,]), site_info_low$Predator_Occupancy, simplify = FALSE))
    
    obs_covs <- list(DOY_scaled = DOY_scaled,
                     days_scaled = days_scaled,
                     pred_occupancy = pred_occupancy) 
    
    # create object with all our data and covariates
    umf <- unmarkedFrameOccu(y = hist_low_prey, # Encounter history, must be a data frame or matrix
                             siteCovs = site_covs, # Site covariates, must be a data frame
                             obsCovs = obs_covs) # Observer covariates, must be list of data frames or matrices
    
    prey_model_low <- occu(~ days_scaled + pred_occupancy ~ Predator_Occupancy + (1 | Array_Year), data = umf) 
  
    summary_prey_low <- summary(prey_model_low)
    
    # predator occupancy high ####
    
    # filter to high human disturbance for observations and sites
    observations_high <- filter(obs_within_range, Humans_Per_Camera_Per_Day >= median)
    site_info_high <- filter(deps_within_range, Humans_Per_Camera_Per_Day >= median)
    
    # CREATE ENCOUNTER HISTORY FOR PREDATOR HIGH
    n_sites <- length(unique(observations_high$Site_Name))
    sampling_int <- 7
    max_events <- ceiling((max(observations_high$Survey_Nights))/sampling_int)
    hist_high_pred <- matrix(NA, ncol = max_events, nrow = n_sites)
    colnames(hist_high_pred) <- paste0("V",seq(1,max_events))
    
    # Add a column for site name
    Site <- c(unique(observations_high$Site_Name))
    hist_high_pred <- cbind(hist_high_pred, Site)
    
    # Copy empty dataframe so we can fill it with other covariates
    
    hist_high_pred <- as.data.frame(hist_high_pred)
    DOY_high_pred <- hist_high_pred
    survey_days_high_pred <- hist_high_pred
    
    # this took ~ 5 min
    for(i in 1:length(unique(observations_high$Site_Name))){
      
      site_i <- unique(observations_high$Site_Name) [i]
      cat("Starting site ",site_i,"\n") # tells you how far along you are
      data_i <- observations_high[which(observations_high$Site_Name == site_i & 
                                          observations_high$Species_Name == pred_name), ]
      
      start_i <- site_info_high$Start_Date[which(site_info_high$Site==site_i)]
      end_i <- site_info_high$End_Date[which(site_info_high$Site==site_i)]
      days_i <- site_info_high$Survey_Nights[which(site_info_high$Site==site_i)]
      
      events_i <- as.numeric(ceiling(days_i/sampling_int))
      # I used ceiling() because if you have 4.3 sampling
      # events you'll want to consider that as 5 sampling events, 4 complete
      # sampling events and one partial event. 
      
      # Now loop through these sampling events and pull out any detections
      # that occur within a given sampling event.
      
      # Do the first event outside the loop so that you can use start_i as the start of the first event.
      start_1 <- start_i
      end_1 <- as.Date(start_1) + sampling_int - 1
      
      # Ask if your camera data has any detections within this interval.
      data_1 <- which(as.Date(data_i$Local_Date) >= start_1 & as.Date(data_i$Local_Date) <= end_1) 
      
      # Is there at least one detection?
      if(length(data_1) > 0) {  # If so, fill in hist_high_pred 
        
        # Put the data in the first column since that will always represent your first detection
        hist_high_pred[which(hist_high_pred$Site==site_i),paste0("V",1)] <- 1 # Detected
        
        # NOTE: You could modify this code to put the actual number of detections
        # into EN, instead of just 1 or 0, and then convert to 0/1 later. That might
        # be useful if you are interested in the number of detections. 
        
      } else { # If not, fill in your hist_high_pred with a zero.=
        hist_high_pred[which(hist_high_pred$Site==site_i),paste0("V",1)] <- 0 # Not detected
        
      }
      
      
      if(end_1 > end_i){
        s_days_1 <- as.Date(end_i) - as.Date(start_1) + 1
      } else {
        s_days_1 <- as.Date(end_1) - as.Date(start_1) + 1
      } # get number of actual survey days
      
      survey_days_high_pred[which(hist_high_pred$Site==site_i),paste0("V",1)] <- s_days_1 
      # add number of survey days to surveys df
      DOY_high_pred[which(hist_high_pred$Site==site_i),paste0("V",1)] <- yday(as.Date(start_1) + (as.numeric(s_days_1)/2)) 
      # calculate day of year and add to DOY_high_pred df
      
      
      
      if(days_i > 7) { 
        # we need to do this to tell it to ONLY loop through other sampling events 
        # if there is more than one sampling event (if days_i is greater than 7)
        
        # Now start looping through the other sampling events
        for(j in 2:events_i){
          
          start_j <- as.Date(start_1) + ((j - 1) * sampling_int)
          # Compare this start_j against end_1 to be sure that the j-th event starts as soon as the j-1th event ends
          end_j <- as.Date(start_j) + sampling_int - 1
          
          data_j <- which(as.Date(data_i$Local_Date) >= as.Date(start_j) & 
                            as.Date(data_i$Local_Date) <= as.Date(end_j))
          
          # the following is the same as above:
          if(length(data_j) > 0){
            hist_high_pred[which(hist_high_pred$Site==site_i),paste0("V",j)] <- 1 # Detected
          } else {
            hist_high_pred[which(hist_high_pred$Site==site_i),paste0("V",j)] <- 0 # Not detected
          }
          
          if(end_j > end_i){
            s_days_j <- as.Date(end_i) - as.Date(start_j) + 1
          } else {
            s_days_j <- as.Date(end_j) - as.Date(start_j) + 1
          }
          
          survey_days_high_pred[which(hist_high_pred$Site==site_i),paste0("V",j)] <- s_days_j
          DOY_high_pred[which(hist_high_pred$Site==site_i),paste0("V",j)] <- yday(as.Date(start_j) + (as.numeric(s_days_j)/2))
          
        }
        
      } else { 
        
        next
        
      }
      
    }
    
    # make sure year is a character not a number!!
    site_info_high$Year <- as.character(site_info_high$Year)
    
    # OCCUPANCY ANALYSIS OF PREDATOR HIGH
    
    # load and format covariates
    # z score standardize covariates
    site_info_high$Humans_Per_Camera_Per_Day <- scale((site_info_high$Humans_Per_Camera_Per_Day))
    site_info_high$Disturbance <- scale(as.matrix(site_info_high$Disturbance))
    
    # observation level covariates we have to do manually since "scale" only scales column by column
    DOY_high_pred <- data.frame(lapply(DOY_high_pred, as.numeric))
    DOY_high_pred <- as.matrix(DOY_high_pred[,grep("V",colnames(DOY_high_pred))])
    mean_DOY <- mean(as.vector(DOY_high_pred), na.rm = T)
    sd_DOY <- sd(as.vector(DOY_high_pred), na.rm = T)
    DOY_scaled <- (DOY_high_pred - mean_DOY)/sd_DOY
    
    survey_days_high_pred <- data.frame(lapply(survey_days_high_pred, as.numeric))
    survey_days_high_pred <- as.matrix(survey_days_high_pred[,grep("V",colnames(survey_days_high_pred))])
    mean_days <- mean(as.vector((survey_days_high_pred)), na.rm = T)
    sd_days <- sd(as.vector((survey_days_high_pred)), na.rm = T)
    days_scaled <- (survey_days_high_pred - mean_days)/sd_days
    
    # pull out data
    hist_high_pred <- hist_high_pred[,grep("V",colnames(hist_high_pred))]
    hist_high_pred[] <- lapply(hist_high_pred, as.numeric)
    
    site_covs <- as.data.frame(site_info_high[,c("Humans_Per_Camera_Per_Day", 
                                                 "Disturbance", 
                                                 "Array_Year", 
                                                 "Year")]) 
    site_covs <- site_covs %>%
      rename(
        Humans = "Humans_Per_Camera_Per_Day",
        Disturbance = "Disturbance")
    
    obs_covs <- list(DOY_scaled = DOY_scaled,
                     days_scaled = days_scaled) 
    
    # create object with all our data and covariates
    umf <- unmarkedFrameOccu(y = hist_high_pred, # Encounter history, must be a data frame or matrix
                             siteCovs = site_covs, # Site covariates, must be a data frame
                             obsCovs = obs_covs) # Observer covariates, must be list of data frames or matrices
    
    pred_model_high <- occu(~ days_scaled ~ 1 + (1 | Array_Year), data = umf) 
    
    summary(pred_model_high)
    
    # PULL OUT FINITE OCCUPANCY ESTIMATES FOR USE IN HIGH PREY
    # grab predator occupancy by site
    pred_site <- ranef(pred_model_high) # this should preserve the order of the sites
    # pred_site
    
    # pull out means
    means <- bup(pred_site, stat = "mean")
    head(means) # this should look like the first few "means" under "pred_site"
    length(means) # this should match the number of rows of all other covariates
    
    # add onto site-level covariates for use in prey
    site_info_high$Predator_Occupancy <- means
    
    # convert to dataframe
    # check what each column is
    print(sapply(site_info_high, class))
    
    # Convert matrix column
    if (is.matrix(site_info_high$Humans_Per_Camera_Per_Day)) {
      matrix_col_df <- as.data.frame(site_info_high$Humans_Per_Camera_Per_Day)
      colnames(matrix_col_df) <- paste("Humans_Per_Camera_Per_Day", 1:ncol(matrix_col_df), sep = "_")
      site_info_high <- cbind(site_info_high, matrix_col_df)
      site_info_high$Humans_Per_Camera_Per_Day <- NULL  # Remove the original matrix column
    }
    
    if (is.matrix(site_info_high$Disturbance)) {
      matrix_col_df <- as.data.frame(site_info_high$Disturbance)
      colnames(matrix_col_df) <- paste("Disturbance", 1:ncol(matrix_col_df), sep = "_")
      site_info_high <- cbind(site_info_high, matrix_col_df)
      site_info_high$Disturbance <- NULL  # Remove the original matrix column
    }
    
    # Check the class of each column again
    print(sapply(site_info_high, class))
    
    # prey occupancy high ####
    # CREATE ENCOUNTER HISTORY FOR PREY HIGH
    n_sites <- length(unique(observations_high$Site_Name))
    sampling_int <- 7
    max_events <- ceiling((max(observations_high$Survey_Nights))/sampling_int)
    hist_high_prey <- matrix(NA, ncol = max_events, nrow = n_sites)
    colnames(hist_high_prey) <- paste0("V",seq(1,max_events))
    
    # Add a column for site name
    Site <- c(unique(observations_high$Site_Name))
    hist_high_prey <- cbind(hist_high_prey, Site)
    
    # Copy empty dataframe so we can fill it with other covariates
    
    hist_high_prey <- as.data.frame(hist_high_prey)
    DOY_high_prey <- hist_high_prey
    survey_days_high_prey <- hist_high_prey
    
    # this took ~ 5 min
    for(i in 1:length(unique(observations_high$Site_Name))){
      
      site_i <- unique(observations_high$Site_Name) [i]
      cat("Starting site ",site_i,"\n") # tells you how far along you are
      data_i <- observations_high[which(observations_high$Site_Name == site_i & 
                                          observations_high$Species_Name == prey_name), ]
      
      start_i <- site_info_high$Start_Date[which(site_info_high$Site==site_i)]
      end_i <- site_info_high$End_Date[which(site_info_high$Site==site_i)]
      days_i <- site_info_high$Survey_Nights[which(site_info_high$Site==site_i)]
      
      events_i <- as.numeric(ceiling(days_i/sampling_int))
      # I used ceiling() because if you have 4.3 sampling
      # events you'll want to consider that as 5 sampling events, 4 complete
      # sampling events and one partial event. 
      
      # Now loop through these sampling events and pull out any detections
      # that occur within a given sampling event.
      
      # Do the first event outside the loop so that you can use start_i as the start of the first event.
      start_1 <- start_i
      end_1 <- as.Date(start_1) + sampling_int - 1
      
      # Ask if your camera data has any detections within this interval.
      data_1 <- which(as.Date(data_i$Local_Date) >= start_1 & as.Date(data_i$Local_Date) <= end_1) 
      
      # Is there at least one detection?
      if(length(data_1) > 0) {  # If so, fill in hist_high_prey 
        
        # Put the data in the first column since that will always represent your first detection
        hist_high_prey[which(hist_high_prey$Site==site_i),paste0("V",1)] <- 1 # Detected
        
        # NOTE: You could modify this code to put the actual number of detections
        # into EN, instead of just 1 or 0, and then convert to 0/1 later. That might
        # be useful if you are interested in the number of detections. 
        
      } else { # If not, fill in your hist_high_prey with a zero.=
        hist_high_prey[which(hist_high_prey$Site==site_i),paste0("V",1)] <- 0 # Not detected
        
      }
      
      
      if(end_1 > end_i){
        s_days_1 <- as.Date(end_i) - as.Date(start_1) + 1
      } else {
        s_days_1 <- as.Date(end_1) - as.Date(start_1) + 1
      } # get number of actual survey days
      
      survey_days_high_prey[which(hist_high_prey$Site==site_i),paste0("V",1)] <- s_days_1 
      # add number of survey days to surveys df
      DOY_high_prey[which(hist_high_prey$Site==site_i),paste0("V",1)] <- yday(as.Date(start_1) + (as.numeric(s_days_1)/2)) 
      # calculate day of year and add to DOY_high_prey df
      
      
      
      if(days_i > 7) { 
        # we need to do this to tell it to ONLY loop through other sampling events 
        # if there is more than one samping event (if days_i is greater than 7)
        
        
        # Now start looping through the other sampling events
        for(j in 2:events_i){
          
          start_j <- as.Date(start_1) + ((j - 1) * sampling_int)
          # Compare this start_j against end_1 to be sure that the j-th event starts as soon as the j-1th event ends
          end_j <- as.Date(start_j) + sampling_int - 1
          
          data_j <- which(as.Date(data_i$Local_Date) >= as.Date(start_j) & 
                            as.Date(data_i$Local_Date) <= as.Date(end_j))
          
          # the following is the same as above:
          if(length(data_j) > 0){
            hist_high_prey[which(hist_high_prey$Site==site_i),paste0("V",j)] <- 1 # Detected
          } else {
            hist_high_prey[which(hist_high_prey$Site==site_i),paste0("V",j)] <- 0 # Not detected
          }
          
          if(end_j > end_i){
            s_days_j <- as.Date(end_i) - as.Date(start_j) + 1
          } else {
            s_days_j <- as.Date(end_j) - as.Date(start_j) + 1
          }
          
          survey_days_high_prey[which(hist_high_prey$Site==site_i),paste0("V",j)] <- s_days_j
          DOY_high_prey[which(hist_high_prey$Site==site_i),paste0("V",j)] <- yday(as.Date(start_j) + (as.numeric(s_days_j)/2))
          
        }
        
      } else { 
        
        next
        
      }
      
    }
    
    # make sure year is a character not a number!!
    site_info_high$Year <- as.character(site_info_high$Year)
    
    # OCCUPANCY ANALYSIS OF PREY HIGH
    
    # load and format covariates
    # z score standardize covariates
    site_info_high$Humans_Per_Camera_Per_Day <- scale((site_info_high$Humans_Per_Camera_Per_Day))
    site_info_high$Disturbance <- scale(as.matrix(site_info_high$Disturbance))
    site_info_high$Predator_Occupancy <- scale(as.matrix(site_info_high$Predator_Occupancy))
    
    # observation level covariates we have to do manually since "scale" only scales column by column
    DOY_high_prey <- data.frame(lapply(DOY_high_prey, as.numeric))
    DOY_high_prey <- as.matrix(DOY_high_prey[,grep("V",colnames(DOY_high_prey))])
    mean_DOY <- mean(as.vector(DOY_high_prey), na.rm = T)
    sd_DOY <- sd(as.vector(DOY_high_prey), na.rm = T)
    DOY_scaled <- (DOY_high_prey - mean_DOY)/sd_DOY
    
    survey_days_high_prey <- data.frame(lapply(survey_days_high_prey, as.numeric))
    survey_days_high_prey <- as.matrix(survey_days_high_prey[,grep("V",colnames(survey_days_high_prey))])
    mean_days <- mean(as.vector((survey_days_high_prey)), na.rm = T)
    sd_days <- sd(as.vector((survey_days_high_prey)), na.rm = T)
    days_scaled <- (survey_days_high_prey - mean_days)/sd_days
    
    # pull out data
    hist_high_prey <- hist_high_prey[,grep("V",colnames(hist_high_prey))]
    hist_high_prey[] <- lapply(hist_high_prey, as.numeric)
    
    site_covs <- as.data.frame(site_info_high[,c("Humans_Per_Camera_Per_Day", 
                                                 "Disturbance", 
                                                 "Array_Year", 
                                                 "Year",
                                                 "Predator_Occupancy")]) 
    site_covs <- site_covs %>%
      rename(
        Humans = "Humans_Per_Camera_Per_Day",
        Disturbance = "Disturbance")
    
    # make a new dataframe out of predator occupancy with same dimensions as other observation covariates
    pred_occupancy <- data.frame(replicate(length(DOY_scaled[1,]), site_info_high$Predator_Occupancy, simplify = FALSE))

    obs_covs <- list(DOY_scaled = DOY_scaled,
                     days_scaled = days_scaled,
                     pred_occupancy = pred_occupancy) 
    
    # create object with all our data and covariates
    umf <- unmarkedFrameOccu(y = hist_high_prey, # Encounter history, must be a data frame or matrix
                             siteCovs = site_covs, # Site covariates, must be a data frame
                             obsCovs = obs_covs) # Observer covariates, must be list of data frames or matrices
    
    prey_model_high <- occu(~ days_scaled + pred_occupancy ~ Predator_Occupancy + (1 | Array_Year), data = umf)

    summary_prey_high <- summary(prey_model_high)
    
    # set row number for adding results to results dataframe
    if (k == 1) {
      m <- (l)
    } else {
      m <- (l + 9)
    }
    
    # calculate CI
    lower_CI_low <- summary_prey_low$state$Estimate[2] - (summary_prey_low$state$SE[2] * 1.96)
    upper_CI_low <- summary_prey_low$state$Estimate[2] + (summary_prey_low$state$SE[2] * 1.96)
    lower_CI_high<- summary_prey_high$state$Estimate[2] - (summary_prey_high$state$SE[2] * 1.96)
    upper_CI_high<- summary_prey_high$state$Estimate[2] + (summary_prey_high$state$SE[2] * 1.96)
    
    # pull out values
    effect_pred_on_prey_low <- summary_prey_low$state$Estimate[2]
    p_value_effect_pred_on_prey_low <- summary_prey_low$state$`P(>|z|)`[2]
    effect_pred_on_prey_high <- summary_prey_high$state$Estimate[2]
    p_value_effect_pred_on_prey_high <- summary_prey_high$state$`P(>|z|)`[2]
  
    
    # fill in results dataframe
    results_occ[m, 1] <- pred_name
    results_occ[m, 2] <- prey_name
    results_occ[m, 3] <- effect_pred_on_prey_low
    results_occ[m, 4] <- p_value_effect_pred_on_prey_low
    results_occ[m, 5] <- effect_pred_on_prey_high
    results_occ[m, 6] <- p_value_effect_pred_on_prey_high
    results_occ[m, 7] <- lower_CI_low
    results_occ[m, 8] <- upper_CI_low
    results_occ[m, 9] <- lower_CI_high
    results_occ[m, 10] <- upper_CI_high
    
    # now detection
    # calculate CI
    lower_CI_low <- summary_prey_low$det$Estimate[3] - (summary_prey_low$det$SE[3] * 1.96)
    upper_CI_low <- summary_prey_low$det$Estimate[3] + (summary_prey_low$det$SE[3] * 1.96)
    lower_CI_high<- summary_prey_high$det$Estimate[3] - (summary_prey_high$det$SE[3] * 1.96)
    upper_CI_high<- summary_prey_high$det$Estimate[3] + (summary_prey_high$det$SE[3] * 1.96)
    # pull out values
    effect_pred_on_prey_low <- summary_prey_low$det$Estimate[3]
    p_value_effect_pred_on_prey_low <- summary_prey_low$det$`P(>|z|)`[3]
    effect_pred_on_prey_high <- summary_prey_high$det$Estimate[3]
    p_value_effect_pred_on_prey_high <- summary_prey_high$det$`P(>|z|)`[3]
    
    # fill in results dataframe
    results_det[m, 1] <- pred_name
    results_det[m, 2] <- prey_name
    results_det[m, 3] <- effect_pred_on_prey_low
    results_det[m, 4] <- p_value_effect_pred_on_prey_low
    results_det[m, 5] <- effect_pred_on_prey_high
    results_det[m, 6] <- p_value_effect_pred_on_prey_high
    results_det[m, 7] <- lower_CI_low
    results_det[m, 8] <- upper_CI_low
    results_det[m, 9] <- lower_CI_high
    results_det[m, 10] <- upper_CI_high
    
  }
}

results_occ$Difference <- results_occ$overlap_high - results_occ$overlap_low
results_occ$Prey_Type <- c("herbivore",
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
results_occ$Significant <- ifelse(results_occ$upper_CI_low >= results_occ$lower_CI_high & 
                                    results_occ$lower_CI_low <= results_occ$upper_CI_high, 
                              "No", "Yes")
results_occ <- results_occ %>%
  mutate(Trend = if_else(Difference < 0, "decreasing", 
                         if_else(Difference > 0, "increasing", "no change")))
results_occ$Trend <- if_else(results_occ$Significant == "No",
                         paste("slightly", results_occ$Trend), paste(results_occ$Trend))
# write results as csv
write_csv(results_occ, "results/pred_prey_occupancy_with_detection_results.csv")

results_det$Difference <- results_det$overlap_high - results_det$overlap_low
results_det$Prey_Type <- c("herbivore",
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
results_det$Significant <- ifelse(results_det$upper_CI_low >= results_det$lower_CI_high & 
                                    results_det$lower_CI_low <= results_det$upper_CI_high, 
                                  "No", "Yes")
results_det <- results_det %>%
  mutate(Trend = if_else(Difference < 0, "decreasing", 
                         if_else(Difference > 0, "increasing", "no change")))
results_det$Trend <- if_else(results_det$Significant == "No",
                             paste("slightly", results_det$Trend), paste(results_det$Trend))
# write results as csv
write_csv(results_det, "results/pred_prey_detection_results.csv")

