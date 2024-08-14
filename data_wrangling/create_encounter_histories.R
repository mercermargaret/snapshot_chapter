# create encounter histories
# margaret mercer (initial code provided by javan bauder)
# july 22, 2024

# the point: Estimate the likelihood of pumas and wolves of being found in
# areas as a function of the human presence in that area

# clear workspace
rm(list=ls())

library(tidyverse)
library(lubridate)
library(sf)

# import and wrangle data ####
# let's see if we can merge these in a way that keeps ALL site names and just has a row of "NA"s if there were no pics.
deployments <- read.csv("data/five_year_deployments.csv") 
observations <- read.csv("../data_too_big/five_year_observation_data.csv") 
joined <- left_join(deployments, observations, by = "Site_Name")
joined$Array <- joined$Array.x
joined$Latitude <- joined$Latitude.x
joined$Longitude <- joined$Longitude.x
joined$Survey_Nights <- joined$Survey_Nights.x
joined$Habitat <- joined$Habitat.x
joined$Development_Level <- joined$Development_Level.x
joined$Disturbance <- joined$Disturbance.x
joined$Humans_Per_Camera_Per_Day <- joined$Humans_Per_Camera_Per_Day.x
data <- subset(joined, select = c("record_ID", 
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
# cool, so merging like that keeps the rows for which there are NO observations?

# geographic subsetting of "data" and "deployments" to only the species range
range <- st_read('data/subset_shape_files/Puma')
sf_use_s2(FALSE)
# cut down so it's only cameras in range of animal
# Convert df to an sf object
points_sf <- st_make_valid(st_as_sf(data, coords = c("Longitude", "Latitude"), crs = st_crs(range)))
st_is_valid(points_sf, reason=TRUE)
sf_use_s2(TRUE)
# Perform the point-in-polygon test
inside <- st_within(points_sf, range, sparse = FALSE)
# Extract rows from df that are inside the polygon
data_within_range <- data[which(inside[,1]),]
# data_within_range now contains only the rows where coordinates fall inside the polygon


# and trim deployments down too
sf_use_s2(FALSE)
points_sf_dep <- st_make_valid(st_as_sf(deployments, coords = c("Longitude", "Latitude"), crs = st_crs(range)))
st_is_valid(points_sf_dep, reason=TRUE)
sf_use_s2(TRUE)
inside_dep <- st_within(points_sf_dep, range, sparse = FALSE)
deployments_within_range <- deployments[which(inside_dep[,1]),]


# Creating encounter history ####
n_sites <- length(unique(data_within_range$Site_Name))
sampling_int <- 7
max_events <- ceiling((max(data_within_range$Survey_Nights))/sampling_int)
hist <- matrix(NA, ncol = max_events, nrow = n_sites)
colnames(hist) <- paste0("V",seq(1,max_events))

# Add a column for site name
Site <- c(unique(data_within_range$Site_Name))
hist <- cbind(hist, Site)

# Say you want to model detection as a function of day-of-year
# You can "copy" your empty hist to make a new data frame that will
# store day-of-year for each detection in the format needed by
# unmarked

hist <- as.data.frame(hist)
DOY <- hist
survey_days <- hist

# # code dataframe with 0 and 1 for species of interest
# data_within_range$Species <- ifelse(data_within_range$Species_Name == "Puma concolor", 1, 0)

# this took ~ 5 min
for(i in 1:length(unique(data_within_range$Site_Name))){
  
  site_i <- unique(data_within_range$Site_Name) [i]
  cat("Starting site ",site_i,"\n") # tells you how far along you are
  data_i <- data_within_range[which(data_within_range$Site_Name == site_i & data_within_range$Species_Name == "Puma concolor"), ]
  
  start_i <- deployments_within_range$Start_Date[which(deployments_within_range$Site==site_i)]
  end_i <- deployments_within_range$End_Date[which(deployments_within_range$Site==site_i)]
  days_i <- deployments_within_range$Survey_Nights[which(deployments_within_range$Site==site_i)]

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

# end_j = calculated end date
# end_i = TRUE end date
# start_j = TRUE start date of this sampling occasion
# if end_j > end_i, we want to use end_i instead of end_j


# qc and troubleshooting ####

# which(unique(data_within_range$Site_Name) == "FL_Forest_DeLuca_011 09/01/2022_2022") # get i for a particular site
# hist[which(hist$Site==site_i), ] # confirm site

# # get total number of 1s in dataframe (how many observation periods contained the species of interest)
# total_ones <- sum(hist == "1", na.rm = TRUE)
# total_ones 
# 
# # let's filter to only data with the species of interest and then get the unique site numbers there
# species <- subset(data_within_range, Species_Name == "Puma concolor")
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

# write csvs ####

write.csv(hist, "data/encounter_histories.csv")
write.csv(DOY, "data/day_of_year.csv")
write.csv(survey_days, "data/survey_days.csv")
write.csv(deployments_within_range, "data/site_info.csv")

