# prey occupancy high
# margaret mercer
# august 5, 2024

# clear workspace
rm(list=ls())

# load packages
library(lubridate)
library(sf)
library(tidyverse)
library(unmarked)
library(AICcmodavg)
library(ggplot2)
library(TMB)

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

# geographic subsetting of "observations_all" and "deployments_all" to only the species pair overlapping range
pred_range <- st_read('data/subset_shape_files/Puma')
prey_range <- st_read("data/subset_shape_files/Whitetail")

sf_use_s2(FALSE) # switch off spherical geometry
range_overlap <- st_intersection(prey_range, pred_range) # now calculate overlap
# make sure everything's valid
points_sf_obs <- st_make_valid(st_as_sf(observations_all, 
                                        coords = c("Longitude", "Latitude"), 
                                        crs = st_crs(range_overlap)))
st_is_valid(points_sf_obs, reason=TRUE)
sf_use_s2(TRUE)
inside_obs <- st_within(points_sf_obs, range_overlap, sparse = FALSE) # Perform the point-in-polygon test
obs_within_range <- observations_all[which(inside_obs[,1]),] # Extract rows from df that are inside the polygon
# obs_within_range now contains only the rows where coordinates fall inside both predator and prey range


# and trim deployments down too
sf_use_s2(FALSE)
points_sf_deps <- st_make_valid(st_as_sf(deployments_all, 
                                         coords = c("Longitude", "Latitude"), 
                                         crs = st_crs(range_overlap)))
st_is_valid(points_sf_deps, reason=TRUE)
sf_use_s2(TRUE)
inside_deps <- st_within(points_sf_deps, range_overlap, sparse = FALSE)
deps_within_range <- deployments_all[which(inside_deps[,1]),]

# now slice and dice
# only include deployment AND observation data from UPPER half of human activity!
# split down median of SITES, not observations

# find median dist and assign to an object
median <- median(deps_within_range$Humans_Per_Camera_Per_Day)

# filter to high human disturbance for observations and sites
observations <- filter(obs_within_range, Humans_Per_Camera_Per_Day >= median)
site_info <- filter(deps_within_range, Humans_Per_Camera_Per_Day >= median)

# create encounter history for prey ####
n_sites <- length(unique(observations$Site_Name))
sampling_int <- 7
max_events <- ceiling((max(observations$Survey_Nights))/sampling_int)
hist <- matrix(NA, ncol = max_events, nrow = n_sites)
colnames(hist) <- paste0("V",seq(1,max_events))

# Add a column for site name
Site <- c(unique(observations$Site_Name))
hist <- cbind(hist, Site)

# Copy empty dataframe so we can fill it with other covariates

hist <- as.data.frame(hist)
DOY <- hist
survey_days <- hist

# this took ~ 5 min
for(i in 1:length(unique(observations$Site_Name))){
  
  site_i <- unique(observations$Site_Name) [i]
  cat("Starting site ",site_i,"\n") # tells you how far along you are
  data_i <- observations[which(observations$Site_Name == site_i & 
                                 observations$Species_Name == "Odocoileus virginianus"), ]
  
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
  if(length(data_1) > 0) {  # If so, fill in hist. 
    
    # Put the data in the first column since that will always represent your first detection
    hist[which(hist$Site==site_i),paste0("V",1)] <- 1 # Detected
    
    # NOTE: You could modify this code to put the actual number of detections
    # into EN, instead of just 1 or 0, and then convert to 0/1 later. That might
    # be useful if you are interested in the number of detections. 
    
  } else { # If not, fill in your hist with a zero.=
    hist[which(hist$Site==site_i),paste0("V",1)] <- 0 # Not detected
    
  }
  
  
  if(end_1 > end_i){
    s_days_1 <- as.Date(end_i) - as.Date(start_1) + 1
  } else {
    s_days_1 <- as.Date(end_1) - as.Date(start_1) + 1
  } # get number of actual survey days
  
  survey_days[which(hist$Site==site_i),paste0("V",1)] <- s_days_1 
  # add number of survey days to surveys df
  DOY[which(hist$Site==site_i),paste0("V",1)] <- yday(as.Date(start_1) + (as.numeric(s_days_1)/2)) 
  # calculate day of year and add to DOY df
  
  
  
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
        hist[which(hist$Site==site_i),paste0("V",j)] <- 1 # Detected
      } else {
        hist[which(hist$Site==site_i),paste0("V",j)] <- 0 # Not detected
      }
      
      if(end_j > end_i){
        s_days_j <- as.Date(end_i) - as.Date(start_j) + 1
      } else {
        s_days_j <- as.Date(end_j) - as.Date(start_j) + 1
      }
      
      survey_days[which(hist$Site==site_i),paste0("V",j)] <- s_days_j
      DOY[which(hist$Site==site_i),paste0("V",j)] <- yday(as.Date(start_j) + (as.numeric(s_days_j)/2))
      
    }
    
  } else { 
    
    next
    
  }
  
}

# make sure year is a character not a number!!
site_info$Year <- as.character(site_info$Year)

# end_j = calculated end date
# end_i = TRUE end date
# start_j = TRUE start date of this sampling occasion
# if end_j > end_i, we want to use end_i instead of end_j


# qc and troubleshooting

# which(unique(observations$Site_Name) == "FL_Forest_DeLuca_011 09/01/2022_2022") # get i for a particular site
# hist[which(hist$Site==site_i), ] # confirm site

# # get total number of 1s in dataframe (how many observation periods contained the species of interest)
# total_ones <- sum(hist == "1", na.rm = TRUE)
# total_ones 
# 
# # let's filter to only data with the species of interest and then get the unique site numbers there
# species <- subset(observations, Species_Name == "species")
# species_sites <- unique(species$Site_Name)
# length(species_sites)
# 
# # and the number of times it says the species of interest are observed
# sum(hist == "1", na.rm = TRUE)
# 
# # now number of rows with at least one "1"
# rows_with_one <- hist[apply(hist, 1, function(row) any(row == 1, na.rm = TRUE)), ]
# length(rows_with_one$Site)
# 
# # compare rows_with_one$Site to species_sites$Site_Name
# sites_1 <- table(unique(species$Site_Name))
# length(sites_1)
# 
# sites_2 <- table(unique(rows_with_one$Site))
# length(sites_2)
# 
# df_1 <- as.data.frame(sites_1)
# df_2 <- as.data.frame(sites_2)
# 
# anti_join(df_1, df_2, by = "Var1") # what rows are in df_1 but not df_2
# anti_join(df_2, df_1, by = "Var1") # what rows are in df_2 but not df_1

# # test for cameras for which local_date is either larger than the end date or smaller than the start date
# observations_outside_date_range <- data[which(as.Date(data$Local_Date) > as.Date(data$End_Date) |
#                                as.Date(data$Local_Date) < as.Date(data$Start_Date)), ]

# # write csvs

write.csv(hist, "data/hist_high_prey.csv")
write.csv(DOY, "data/DOY_high_prey.csv")
write.csv(survey_days, "data/survey_days_high_prey.csv")

# occupancy analysis of prey ####

# clear workspace
rm(list=ls())

# load and format covariates

hist <- read.csv("data/hist_high_prey.csv")
DOY <- read.csv("data/DOY_high_prey.csv")
survey_days <- read.csv("data/survey_days_high_prey.csv")
site_info <- read.csv("data/site_info_high_prey.csv") # we made this in the "predator_occupancy_high" file

# z score standardize covariates
site_info$Humans_Per_Camera_Per_Day <- scale((site_info$Humans_Per_Camera_Per_Day))
site_info$Disturbance <- scale(as.matrix(site_info$Disturbance))
site_info$Predator_Occupancy <- scale(as.matrix(site_info$Predator_Occupancy))

# observation level covariates we have to do manually since "scale" only scales column by column
DOY <- data.frame(lapply(DOY, as.numeric))
DOY <- as.matrix(DOY[,grep("V",colnames(DOY))])
mean_DOY <- mean(as.vector(DOY), na.rm = T)
sd_DOY <- sd(as.vector(DOY), na.rm = T)
DOY_scaled <- (DOY - mean_DOY)/sd_DOY

survey_days <- data.frame(lapply(survey_days, as.numeric))
survey_days <- as.matrix(survey_days[,grep("V",colnames(survey_days))])
mean_days <- mean(as.vector((survey_days)), na.rm = T)
sd_days <- sd(as.vector((survey_days)), na.rm = T)
days_scaled <- (survey_days - mean_days)/sd_days

# check for inconsistent number of NAs (should be the same for all four dfs)
# sum(is.na(DOY))
# sum(is.na(DOY_scaled))
# sum(is.na(survey_days))
# sum(is.na(days_scaled))

# pull out data
observations <- hist[,grep("V",colnames(hist))]

site_covs <- as.data.frame(site_info[,c("Humans_Per_Camera_Per_Day", 
                                        "Disturbance", 
                                        "Array", 
                                        "Year",
                                        "Predator_Occupancy")]) 
site_covs <- site_covs %>%
  rename(
    Humans = "Humans_Per_Camera_Per_Day",
    Disturbance = "Disturbance")
# Thought: should we call each array "array_year" in case the some of the arrays 
# had the same name across years?

obs_covs <- list(DOY_scaled = DOY_scaled,
                 days_scaled = days_scaled) 

# create object with all our data and covariates
umf <- unmarkedFrameOccu(y = observations, # Encounter history, must be a data frame or matrix
                         siteCovs = site_covs, # Site covariates, must be a data frame
                         obsCovs = obs_covs) # Observer covariates, must be list of data frames or matrices

prey_model <- occu(~ days_scaled ~ Predator_Occupancy + (1 | Array), data = umf) 
# what do here? which value do we pull out, if we're accounting for days scaled? How do we deal with random effects?

summary(prey_model)
