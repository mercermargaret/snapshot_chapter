# comparing SPATIAL overlap between pred and prey to see if diff between puma and wolves
# margaret mercer
# march 22 2025

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
  "overlap" = numeric(18), 
  "p-value" = numeric(18), 
  "lower_CI" = numeric(18),
  "upper_CI" = numeric(18),
  stringsAsFactors = FALSE
)




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
      observations <- observations_all[which(inside_obs[,1]),]
      
      # trim deployments too
      
      sf_use_s2(FALSE)
      points_sf_deps <- st_make_valid(st_as_sf(deployments_all, 
                                               coords = c("Longitude", "Latitude"), 
                                               crs = st_crs(range_overlap)))
      st_is_valid(points_sf_deps, reason=TRUE)
      sf_use_s2(TRUE)
      inside_deps <- st_within(points_sf_deps, range_overlap, sparse = FALSE)
      site_info <- deployments_all[which(inside_deps[,1]),]
      
      
    } else {
      
      # switch off spherical geometry
      sf_use_s2(FALSE)
      
      # now calculate overlap
      range_overlap <- st_intersection(prey_range, pred_range)
      st_is_valid(range_overlap, reason=TRUE)
      
      sf_use_s2(TRUE)
      inside_obs <- st_within(points_sf_obs, range_overlap, sparse = FALSE)
      observations <- observations_all[which(inside_obs[,1]),]
      
      # trim deployments too
      
      sf_use_s2(FALSE)
      points_sf_deps <- st_make_valid(st_as_sf(deployments_all, 
                                               coords = c("Longitude", "Latitude"), 
                                               crs = st_crs(range_overlap)))
      st_is_valid(points_sf_deps, reason=TRUE)
      sf_use_s2(TRUE)
      inside_deps <- st_within(points_sf_deps, range_overlap, sparse = FALSE)
      site_info <- deployments_all[which(inside_deps[,1]),]
      
    }
    
    
    # predator occupancy ####
    # CREATE ENCOUNTER HISTORY FOR  PREDATOR
    n_sites <- length(unique(observations$Site_Name))
    sampling_int <- 7
    max_events <- ceiling((max(observations$Survey_Nights))/sampling_int)
    hist_pred <- matrix(NA, ncol = max_events, nrow = n_sites)
    colnames(hist_pred) <- paste0("V",seq(1,max_events))
    
    # Add a column for site name
    Site <- c(unique(observations$Site_Name))
    hist_pred <- cbind(hist_pred, Site)
    
    # Copy empty dataframe so we can fill it with other covariates
    
    hist_pred <- as.data.frame(hist_pred)
    DOY_pred <- hist_pred
    survey_days_pred <- hist_pred
    
    # this took ~ 5 min
    for(i in 1:length(unique(observations$Site_Name))){
      
      site_i <- unique(observations$Site_Name) [i]
      cat("Starting site ",site_i,"\n") # tells you how far along you are
      data_i <- observations[which(observations$Site_Name == site_i & 
                                     observations$Species_Name == pred_name), ]
      
      start_i <- site_info$Start_Date[which(site_info$Site==site_i)]
      end_i <- site_info$End_Date[which(site_info$Site==site_i)]
      days_i <- site_info$Survey_Nights[which(site_info$Site==site_i)]
      
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
      if(length(data_1) > 0) {  # If so, fill in hist_pred 
        
        # Put the data in the first column since that will always represent your first detection
        hist_pred[which(hist_pred$Site==site_i),paste0("V",1)] <- 1 # Detected
        
        # NOTE: You could modify this code to put the actual number of detections
        # into EN, instead of just 1 or 0, and then convert to 0/1 later. That might
        # be useful if you are interested in the number of detections. 
        
      } else { # If not, fill in your hist_pred with a zero.=
        hist_pred[which(hist_pred$Site==site_i),paste0("V",1)] <- 0 # Not detected
        
      }
      
      
      if(end_1 > end_i){
        s_days_1 <- as.Date(end_i) - as.Date(start_1) + 1
      } else {
        s_days_1 <- as.Date(end_1) - as.Date(start_1) + 1
      } # get number of actual survey days
      
      survey_days_pred[which(hist_pred$Site==site_i),paste0("V",1)] <- s_days_1 
      # add number of survey days to surveys df
      DOY_pred[which(hist_pred$Site==site_i),paste0("V",1)] <- yday(as.Date(start_1) + (as.numeric(s_days_1)/2)) 
      # calculate day of year and add to DOY_pred df
      
      
      
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
            hist_pred[which(hist_pred$Site==site_i),paste0("V",j)] <- 1 # Detected
          } else {
            hist_pred[which(hist_pred$Site==site_i),paste0("V",j)] <- 0 # Not detected
          }
          
          if(end_j > end_i){
            s_days_j <- as.Date(end_i) - as.Date(start_j) + 1
          } else {
            s_days_j <- as.Date(end_j) - as.Date(start_j) + 1
          }
          
          survey_days_pred[which(hist_pred$Site==site_i),paste0("V",j)] <- s_days_j
          DOY_pred[which(hist_pred$Site==site_i),paste0("V",j)] <- yday(as.Date(start_j) + (as.numeric(s_days_j)/2))
          
        }
        
      } else { 
        
        next
        
      }
      
    }
    
    # make sure year is a character not a number!!
    site_info$Year <- as.character(site_info$Year)
    
    # OCCUPANCY ANALYSIS OF PREDATOR
    # z score standardize covariates
    site_info$Humans_Per_Camera_Per_Day <- scale((site_info$Humans_Per_Camera_Per_Day))
    site_info$Disturbance <- scale(as.matrix(site_info$Disturbance))
    
    # observation level covariates we have to do manually since "scale" only scales column by column
    DOY_pred <- data.frame(lapply(DOY_pred, as.numeric))
    DOY_pred <- as.matrix(DOY_pred[,grep("V",colnames(DOY_pred))])
    mean_DOY <- mean(as.vector(DOY_pred), na.rm = T)
    sd_DOY <- sd(as.vector(DOY_pred), na.rm = T)
    DOY_scaled <- (DOY_pred - mean_DOY)/sd_DOY
    
    survey_days_pred <- data.frame(lapply(survey_days_pred, as.numeric))
    survey_days_pred <- as.matrix(survey_days_pred[,grep("V",colnames(survey_days_pred))])
    mean_days <- mean(as.vector((survey_days_pred)), na.rm = T)
    sd_days <- sd(as.vector((survey_days_pred)), na.rm = T)
    days_scaled <- (survey_days_pred - mean_days)/sd_days
    
    # pull out data
    hist_pred <- hist_pred[,grep("V",colnames(hist_pred))]
    hist_pred[] <- lapply(hist_pred, as.numeric)
    
    # start here
    site_covs <- as.data.frame(site_info[,c("Humans_Per_Camera_Per_Day", 
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
    umf <- unmarkedFrameOccu(y = hist_pred, # Encounter history, must be a data frame or matrix
                             siteCovs = site_covs, # Site covariates, must be a data frame
                             obsCovs = obs_covs) # Observer covariates, must be list of data frames or matrices
    
    pred_model <- occu(~ days_scaled ~ 1 + (1 | Array_Year), data = umf) 
    # what do here? which value do we pull out, if we're accounting for days scaled? How do we deal with random effects?
    
    summary(pred_model)
    
    # PULL OUT PREDATOR  OCCUPANCY ESTIMATES FOR USE IN  PREY
    # grab predator occupancy by site
    pred_site <- ranef(pred_model) # this should preserve the order of the sites
    # pred_site
    
    # pull out means
    means <- bup(pred_site, stat = "mean")
    head(means) # this should look like the first few "means" under "pred_site"
    length(means) # this should match the number of rows of all other covariates
    
    # add onto site-level covariates for use in prey
    site_info$Predator_Occupancy <- means
    
    # convert to dataframe
    # check what each column is
    print(sapply(site_info, class))
    
    # Convert matrix column
    if (is.matrix(site_info$Humans_Per_Camera_Per_Day)) {
      matrix_col_df <- as.data.frame(site_info$Humans_Per_Camera_Per_Day)
      colnames(matrix_col_df) <- paste("Humans_Per_Camera_Per_Day", 1:ncol(matrix_col_df), sep = "_")
      site_info <- cbind(site_info, matrix_col_df)
      site_info$Humans_Per_Camera_Per_Day <- NULL  # Remove the original matrix column
    }
    
    if (is.matrix(site_info$Disturbance)) {
      matrix_col_df <- as.data.frame(site_info$Disturbance)
      colnames(matrix_col_df) <- paste("Disturbance", 1:ncol(matrix_col_df), sep = "_")
      site_info <- cbind(site_info, matrix_col_df)
      site_info$Disturbance <- NULL  # Remove the original matrix column
    }
    
    # Check the class of each column again
    print(sapply(site_info, class))
    
    
    # prey occupancy  ####
    
    # CREATE ENCOUNTER HISTORY FOR  PREY
    n_sites <- length(unique(observations$Site_Name))
    sampling_int <- 7
    max_events <- ceiling((max(observations$Survey_Nights))/sampling_int)
    hist_prey <- matrix(NA, ncol = max_events, nrow = n_sites)
    colnames(hist_prey) <- paste0("V",seq(1,max_events))
    
    # Add a column for site name
    Site <- c(unique(observations$Site_Name))
    hist_prey <- cbind(hist_prey, Site)
    
    # Copy empty dataframe so we can fill it with other covariates
    
    hist_prey <- as.data.frame(hist_prey)
    DOY_prey <- hist_prey
    survey_days_prey <- hist_prey
    
    # this took ~ 5 min
    for(i in 1:length(unique(observations$Site_Name))){
      
      site_i <- unique(observations$Site_Name) [i]
      cat("Starting site ",site_i,"\n") # tells you how far along you are
      data_i <- observations[which(observations$Site_Name == site_i & 
                                         observations$Species_Name == prey_name), ]
      
      start_i <- site_info$Start_Date[which(site_info$Site==site_i)]
      end_i <- site_info$End_Date[which(site_info$Site==site_i)]
      days_i <- site_info$Survey_Nights[which(site_info$Site==site_i)]
      
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
      if(length(data_1) > 0) {  # If so, fill in hist_prey 
        
        # Put the data in the first column since that will always represent your first detection
        hist_prey[which(hist_prey$Site==site_i),paste0("V",1)] <- 1 # Detected
        
        # NOTE: You could modify this code to put the actual number of detections
        # into EN, instead of just 1 or 0, and then convert to 0/1 later. That might
        # be useful if you are interested in the number of detections. 
        
      } else { # If not, fill in your hist_prey with a zero.=
        hist_prey[which(hist_prey$Site==site_i),paste0("V",1)] <- 0 # Not detected
        
      }
      
      
      if(end_1 > end_i){
        s_days_1 <- as.Date(end_i) - as.Date(start_1) + 1
      } else {
        s_days_1 <- as.Date(end_1) - as.Date(start_1) + 1
      } # get number of actual survey days
      
      survey_days_prey[which(hist_prey$Site==site_i),paste0("V",1)] <- s_days_1 
      # add number of survey days to surveys df
      DOY_prey[which(hist_prey$Site==site_i),paste0("V",1)] <- yday(as.Date(start_1) + (as.numeric(s_days_1)/2)) 
      # calculate day of year and add to DOY_prey df
      
      
      
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
            hist_prey[which(hist_prey$Site==site_i),paste0("V",j)] <- 1 # Detected
          } else {
            hist_prey[which(hist_prey$Site==site_i),paste0("V",j)] <- 0 # Not detected
          }
          
          if(end_j > end_i){
            s_days_j <- as.Date(end_i) - as.Date(start_j) + 1
          } else {
            s_days_j <- as.Date(end_j) - as.Date(start_j) + 1
          }
          
          survey_days_prey[which(hist_prey$Site==site_i),paste0("V",j)] <- s_days_j
          DOY_prey[which(hist_prey$Site==site_i),paste0("V",j)] <- yday(as.Date(start_j) + (as.numeric(s_days_j)/2))
          
        }
        
      } else { 
        
        next
        
      }
      
    }
    
    # make sure year is a character not a number!!
    site_info$Year <- as.character(site_info$Year)
    
    # OCCUPANCY ANALYSIS OF  PREY
    
    # load and format covariates
    # z score standardize covariates
    site_info$Humans_Per_Camera_Per_Day <- scale((site_info$Humans_Per_Camera_Per_Day))
    site_info$Disturbance <- scale(as.matrix(site_info$Disturbance))
    site_info$Predator_Occupancy <- scale(as.matrix(site_info$Predator_Occupancy))
    
    # observation level covariates we have to do manually since "scale" only scales column by column
    DOY_prey <- data.frame(lapply(DOY_prey, as.numeric))
    DOY_prey <- as.matrix(DOY_prey[,grep("V",colnames(DOY_prey))])
    mean_DOY <- mean(as.vector(DOY_prey), na.rm = T)
    sd_DOY <- sd(as.vector(DOY_prey), na.rm = T)
    DOY_scaled <- (DOY_prey - mean_DOY)/sd_DOY
    
    survey_days_prey <- data.frame(lapply(survey_days_prey, as.numeric))
    survey_days_prey <- as.matrix(survey_days_prey[,grep("V",colnames(survey_days_prey))])
    mean_days <- mean(as.vector((survey_days_prey)), na.rm = T)
    sd_days <- sd(as.vector((survey_days_prey)), na.rm = T)
    days_scaled <- (survey_days_prey - mean_days)/sd_days
    
    
    # pull out data
    hist_prey <- hist_prey[,grep("V",colnames(hist_prey))]
    hist_prey[] <- lapply(hist_prey, as.numeric)
    
    site_covs <- as.data.frame(site_info[,c("Humans_Per_Camera_Per_Day", 
                                                "Disturbance", 
                                                "Array_Year", 
                                                "Year",
                                                "Predator_Occupancy")]) 
    site_covs <- site_covs %>%
      rename(
        Humans = "Humans_Per_Camera_Per_Day",
        Disturbance = "Disturbance")
    
    # make a new dataframe out of predator occupancy with same dimensions as other observation covariates
    pred_occupancy <- data.frame(replicate(length(DOY_scaled[1,]), site_info$Predator_Occupancy, simplify = FALSE))
    
    obs_covs <- list(DOY_scaled = DOY_scaled,
                     days_scaled = days_scaled,
                     pred_occupancy = pred_occupancy) 
    
    # create object with all our data and covariates
    umf <- unmarkedFrameOccu(y = hist_prey, # Encounter history, must be a data frame or matrix
                             siteCovs = site_covs, # Site covariates, must be a data frame
                             obsCovs = obs_covs) # Observer covariates, must be list of data frames or matrices
    
    prey_model <- occu(~ days_scaled + pred_occupancy ~ Predator_Occupancy + (1 | Array_Year), data = umf) 
    
    summary_prey <- summary(prey_model)
    
    # set row number for adding results to results dataframe
    if (k == 1) {
      m <- (l)
    } else {
      m <- (l + 9)
    }
    
    # calculate CI
    lower_CI <- summary_prey$state$Estimate[2] - (summary_prey$state$SE[2] * 1.96)
    upper_CI <- summary_prey$state$Estimate[2] + (summary_prey$state$SE[2] * 1.96)

    # pull out values
    effect_pred_on_prey <- summary_prey$state$Estimate[2]
    p_value_effect_pred_on_prey <- summary_prey$state$`P(>|z|)`[2]

    
    
    # fill in results dataframe
    results_occ[m, 1] <- pred_name
    results_occ[m, 2] <- prey_name
    results_occ[m, 3] <- effect_pred_on_prey
    results_occ[m, 4] <- p_value_effect_pred_on_prey
    results_occ[m, 5] <- lower_CI
    results_occ[m, 6] <- upper_CI
    

    
  }
}

# errored at wolf/red fox (of COURSE ugh)
# Error in array(x, c(length(x), 1L), if (!is.null(names(x))) list(names(x),  : 
# 'data' must be of a vector type, was 'NULL'

# write results as csv
write_csv(results_occ, "results/puma_vs_wolf_spatial_overlap.csv")


