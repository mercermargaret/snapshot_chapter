# occupancy of each species in low vs high human activity
# margaret mercer
# august 21, 2024

# takes 30 minutes

# load packages
library(lubridate)
library(sf)
library(tidyverse)
library(unmarked)
library(AICcmodavg)
library(ggplot2)
library(TMB)
library(tictoc)

# clear workspace
rm(list=ls())

tic()
# import and wrangle data ####
# let's see if we can merge these in a way that keeps ALL site names and just has a row of "NA"s if there were no pics.
deployments_all <- read.csv("data/five_year_deployments.csv") 
observations_all <- read.csv("../data_too_big/five_year_observation_data.csv") 
joined <- left_join(deployments_all, observations_all, by = "Site_Name")
joined$Array <- joined$Array.x
joined$Latitude <- joined$Latitude.x
joined$Longitude <- joined$Longitude.x
joined$Survey_Nights <- joined$Survey_Nights.x
joined$Habitat <- joined$Habitat.x
joined$Development_Level <- joined$Development_Level.x
joined$Disturbance <- joined$Disturbance.x
joined$Humans_Per_Camera_Per_Day <- joined$Humans_Per_Camera_Per_Day.x
observations_all <- subset(joined, select = c("record_ID", 
                                              "Array", 
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

range_list <- list(puma,
                   wolf,
                   whitetail,
                   mule,
                   elk,
                   moose,
                   coyote,
                   bobcat,
                   raccoon,
                   fox,
                   skunk) 

# create empty dataframe for occupancy values
results <- data.frame(
  "Species" = character(11),
  "Occupancy_Low" = numeric(11), 
  "p-value_low" = numeric(11),
  "Occupancy_High" = numeric(11), 
  "p-value_high" = numeric(11),
  stringsAsFactors = FALSE
)

# begin the for loop ####
# since i and j are already used within this, we'll use k

for (k in 1:length(species_list)) {
  species_name <- species_list[k] # select species from list
  
    cat("Starting: ", species_name)
    
    # geographic subsetting of "observations_all" and "deployments_all" to only the species pair overlapping range
    species_range <- range_list[[k]]
    
    # Convert df to an sf object
    points_sf_obs <- st_make_valid(st_as_sf(observations_all, 
                                            coords = c("Longitude", "Latitude"), 
                                            crs = st_crs(species_range)))
    
      # switch off spherical geometry
      sf_use_s2(FALSE)
      
      sf_use_s2(TRUE)
      inside_obs <- st_within(points_sf_obs, species_range, sparse = FALSE)
      obs_within_range <- observations_all[which(inside_obs[,1]),]
      
      # trim deployments too
      
      sf_use_s2(FALSE)
      points_sf_deps <- st_make_valid(st_as_sf(deployments_all, 
                                               coords = c("Longitude", "Latitude"), 
                                               crs = st_crs(species_range)))
      st_is_valid(points_sf_deps, reason=TRUE)
      sf_use_s2(TRUE)
      inside_deps <- st_within(points_sf_deps, species_range, sparse = FALSE)
      deps_within_range <- deployments_all[which(inside_deps[,1]),]
      
    
    # split down median of SITES, not observations
    # find median dist and assign to an object
    median <- median(deps_within_range$Humans_Per_Camera_Per_Day)
    
    
    # occupancy low ####
    # only include deployment AND observation data from LOWER half of human activity!
    # filter to low human disturbance for observations and sites
    observations_low <- filter(obs_within_range, Humans_Per_Camera_Per_Day < median)
    site_info_low <- filter(deps_within_range, Humans_Per_Camera_Per_Day < median)
    
    # CREATE ENCOUNTER HISTORY FOR LOW SPECIES
    n_sites <- length(unique(observations_low$Site_Name))
    sampling_int <- 7
    max_events <- ceiling((max(observations_low$Survey_Nights))/sampling_int)
    hist_low <- matrix(NA, ncol = max_events, nrow = n_sites)
    colnames(hist_low) <- paste0("V",seq(1,max_events))
    
    # Add a column for site name
    Site <- c(unique(observations_low$Site_Name))
    hist_low <- cbind(hist_low, Site)
    
    # Copy empty dataframe so we can fill it with other covariates
    
    hist_low <- as.data.frame(hist_low)
    DOY_low <- hist_low
    survey_days_low <- hist_low
    
    # this took ~ 5 min
    for(i in 1:length(unique(observations_low$Site_Name))){
      
      site_i <- unique(observations_low$Site_Name) [i]
      cat("Starting site ",site_i,"\n") # tells you how far along you are
      data_i <- observations_low[which(observations_low$Site_Name == site_i & 
                                         observations_low$Species_Name == species_name), ]
      
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
      if(length(data_1) > 0) {  # If so, fill in hist_low 
        
        # Put the data in the first column since that will always represent your first detection
        hist_low[which(hist_low$Site==site_i),paste0("V",1)] <- 1 # Detected
        
        # NOTE: You could modify this code to put the actual number of detections
        # into EN, instead of just 1 or 0, and then convert to 0/1 later. That might
        # be useful if you are interested in the number of detections. 
        
      } else { # If not, fill in your hist_low with a zero.=
        hist_low[which(hist_low$Site==site_i),paste0("V",1)] <- 0 # Not detected
        
      }
      
      
      if(end_1 > end_i){
        s_days_1 <- as.Date(end_i) - as.Date(start_1) + 1
      } else {
        s_days_1 <- as.Date(end_1) - as.Date(start_1) + 1
      } # get number of actual survey days
      
      survey_days_low[which(hist_low$Site==site_i),paste0("V",1)] <- s_days_1 
      # add number of survey days to surveys df
      DOY_low[which(hist_low$Site==site_i),paste0("V",1)] <- yday(as.Date(start_1) + (as.numeric(s_days_1)/2)) 
      # calculate day of year and add to DOY_lowdf
      
      
      
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
            hist_low[which(hist_low$Site==site_i),paste0("V",j)] <- 1 # Detected
          } else {
            hist_low[which(hist_low$Site==site_i),paste0("V",j)] <- 0 # Not detected
          }
          
          if(end_j > end_i){
            s_days_j <- as.Date(end_i) - as.Date(start_j) + 1
          } else {
            s_days_j <- as.Date(end_j) - as.Date(start_j) + 1
          }
          
          survey_days_low[which(hist_low$Site==site_i),paste0("V",j)] <- s_days_j
          DOY_low[which(hist_low$Site==site_i),paste0("V",j)] <- yday(as.Date(start_j) + (as.numeric(s_days_j)/2))
          
        }
        
      } else { 
        
        next
        
      }
      
    }
    
    # make sure year is a character not a number!!
    site_info_low$Year <- as.character(site_info_low$Year)
    
    # OCCUPANCY ANALYSIS OF LOW
    # z score standardize covariates
    site_info_low$Humans_Per_Camera_Per_Day <- scale((site_info_low$Humans_Per_Camera_Per_Day))
    site_info_low$Disturbance <- scale(as.matrix(site_info_low$Disturbance))
    
    # observation level covariates we have to do manually since "scale" only scales column by column
    DOY_low <- data.frame(lapply(DOY_low, as.numeric))
    DOY_low <- as.matrix(DOY_low[,grep("V",colnames(DOY_low))])
    mean_DOY <- mean(as.vector(DOY_low), na.rm = T)
    sd_DOY <- sd(as.vector(DOY_low), na.rm = T)
    DOY_scaled <- (DOY_low - mean_DOY)/sd_DOY
    
    survey_days_low <- data.frame(lapply(survey_days_low, as.numeric))
    survey_days_low <- as.matrix(survey_days_low[,grep("V",colnames(survey_days_low))])
    mean_days <- mean(as.vector((survey_days_low)), na.rm = T)
    sd_days <- sd(as.vector((survey_days_low)), na.rm = T)
    days_scaled <- (survey_days_low - mean_days)/sd_days
    
    # pull out data
    hist_low <- hist_low[,grep("V",colnames(hist_low))]
    hist_low[] <- lapply(hist_low, as.numeric)
    
    # start here
    site_covs <- as.data.frame(site_info_low[,c("Humans_Per_Camera_Per_Day", 
                                                "Disturbance", 
                                                "Array", 
                                                "Year")]) 
    site_covs <- site_covs %>%
      rename(
        Humans = "Humans_Per_Camera_Per_Day",
        Disturbance = "Disturbance")
    # Thought: should we call each array "array_year" in case the some of the arrays 
    # had the same name across years?
    
    obs_covs <- list(DOY_scaled = DOY_scaled,
                     days_scaled = days_scaled) 
    
    # create object with all our data and covariates
    umf <- unmarkedFrameOccu(y = hist_low, # Encounter history, must be a data frame or matrix
                             siteCovs = site_covs, # Site covariates, must be a data frame
                             obsCovs = obs_covs) # Observer covariates, must be list of data frames or matrices
    
    model_low <- occu(~ days_scaled ~ Year + (1 | Array), data = umf) 
    # what do here? which value do we pull out, if we're accounting for days scaled? How do we deal with random effects?
    
    summary_low <- summary(model_low)
    
    # occupancy high ####
    
    # filter to high human disturbance for observations and sites
    observations_high <- filter(obs_within_range, Humans_Per_Camera_Per_Day >= median)
    site_info_high <- filter(deps_within_range, Humans_Per_Camera_Per_Day >= median)
    
    # CREATE ENCOUNTER HISTORY FOR HIGH
    n_sites <- length(unique(observations_high$Site_Name))
    sampling_int <- 7
    max_events <- ceiling((max(observations_high$Survey_Nights))/sampling_int)
    hist_high <- matrix(NA, ncol = max_events, nrow = n_sites)
    colnames(hist_high) <- paste0("V",seq(1,max_events))
    
    # Add a column for site name
    Site <- c(unique(observations_high$Site_Name))
    hist_high <- cbind(hist_high, Site)
    
    # Copy empty dataframe so we can fill it with other covariates
    
    hist_high <- as.data.frame(hist_high)
    DOY_high <- hist_high
    survey_days_high <- hist_high
    
    # this took ~ 5 min
    for(i in 1:length(unique(observations_high$Site_Name))){
      
      site_i <- unique(observations_high$Site_Name) [i]
      cat("Starting site ",site_i,"\n") # tells you how far along you are
      data_i <- observations_high[which(observations_high$Site_Name == site_i & 
                                          observations_high$Species_Name == species_name), ]
      
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
      if(length(data_1) > 0) {  # If so, fill in hist_high 
        
        # Put the data in the first column since that will always represent your first detection
        hist_high[which(hist_high$Site==site_i),paste0("V",1)] <- 1 # Detected
        
        # NOTE: You could modify this code to put the actual number of detections
        # into EN, instead of just 1 or 0, and then convert to 0/1 later. That might
        # be useful if you are interested in the number of detections. 
        
      } else { # If not, fill in your hist_high with a zero.=
        hist_high[which(hist_high$Site==site_i),paste0("V",1)] <- 0 # Not detected
        
      }
      
      
      if(end_1 > end_i){
        s_days_1 <- as.Date(end_i) - as.Date(start_1) + 1
      } else {
        s_days_1 <- as.Date(end_1) - as.Date(start_1) + 1
      } # get number of actual survey days
      
      survey_days_high[which(hist_high$Site==site_i),paste0("V",1)] <- s_days_1 
      # add number of survey days to surveys df
      DOY_high[which(hist_high$Site==site_i),paste0("V",1)] <- yday(as.Date(start_1) + (as.numeric(s_days_1)/2)) 
      # calculate day of year and add to DOY_high df
      
      
      
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
            hist_high[which(hist_high$Site==site_i),paste0("V",j)] <- 1 # Detected
          } else {
            hist_high[which(hist_high$Site==site_i),paste0("V",j)] <- 0 # Not detected
          }
          
          if(end_j > end_i){
            s_days_j <- as.Date(end_i) - as.Date(start_j) + 1
          } else {
            s_days_j <- as.Date(end_j) - as.Date(start_j) + 1
          }
          
          survey_days_high[which(hist_high$Site==site_i),paste0("V",j)] <- s_days_j
          DOY_high[which(hist_high$Site==site_i),paste0("V",j)] <- yday(as.Date(start_j) + (as.numeric(s_days_j)/2))
          
        }
        
      } else { 
        
        next
        
      }
      
    }
    
    # make sure year is a character not a number!!
    site_info_high$Year <- as.character(site_info_high$Year)
    
    # OCCUPANCY ANALYSIS OF HIGH
    
    # load and format covariates
    # z score standardize covariates
    site_info_high$Humans_Per_Camera_Per_Day <- scale((site_info_high$Humans_Per_Camera_Per_Day))
    site_info_high$Disturbance <- scale(as.matrix(site_info_high$Disturbance))
    
    # observation level covariates we have to do manually since "scale" only scales column by column
    DOY_high <- data.frame(lapply(DOY_high, as.numeric))
    DOY_high <- as.matrix(DOY_high[,grep("V",colnames(DOY_high))])
    mean_DOY <- mean(as.vector(DOY_high), na.rm = T)
    sd_DOY <- sd(as.vector(DOY_high), na.rm = T)
    DOY_scaled <- (DOY_high - mean_DOY)/sd_DOY
    
    survey_days_high <- data.frame(lapply(survey_days_high, as.numeric))
    survey_days_high <- as.matrix(survey_days_high[,grep("V",colnames(survey_days_high))])
    mean_days <- mean(as.vector((survey_days_high)), na.rm = T)
    sd_days <- sd(as.vector((survey_days_high)), na.rm = T)
    days_scaled <- (survey_days_high - mean_days)/sd_days
    
    # pull out data
    hist_high <- hist_high[,grep("V",colnames(hist_high))]
    hist_high[] <- lapply(hist_high, as.numeric)
    
    site_covs <- as.data.frame(site_info_high[,c("Humans_Per_Camera_Per_Day", 
                                                 "Disturbance", 
                                                 "Array", 
                                                 "Year")]) 
    site_covs <- site_covs %>%
      rename(
        Humans = "Humans_Per_Camera_Per_Day",
        Disturbance = "Disturbance")
    # Thought: should we call each array "array_year" in case the some of the arrays 
    # had the same name across years?
    
    obs_covs <- list(DOY_scaled = DOY_scaled,
                     days_scaled = days_scaled) 
    
    # create object with all our data and covariates
    umf <- unmarkedFrameOccu(y = hist_high, # Encounter history, must be a data frame or matrix
                             siteCovs = site_covs, # Site covariates, must be a data frame
                             obsCovs = obs_covs) # Observer covariates, must be list of data frames or matrices
    
    model_high <- occu(~ days_scaled ~ Year + (1 | Array), data = umf) 
    # what do here? which value do we pull out, if we're accounting for days scaled? How do we deal with random effects?
    
    summary_high <- summary(model_high)
    
    # fill in results dataframe
    occupancy_low <- summary_low$state$Estimate[1]
    p_value_low <- summary_low$state$`P(>|z|)`[1]
    occupancy_high <- summary_high$state$Estimate[1]
    p_value_high <- summary_high$state$`P(>|z|)`[1]
    
    results[k, 1] <- species_name
    results[k, 2] <- occupancy_low
    results[k, 3] <- p_value_low
    results[k, 4] <- occupancy_high
    results[k, 5] <- p_value_high

}

toc()

results$Difference <- results$Occupancy_High - results$Occupancy_Low

# write results as csv
write_csv(results, "results/low_vs_high_occupancy_modeling_results.csv")
