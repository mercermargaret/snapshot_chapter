# Getting a dataframe of deployments that includes human presence
# Margaret Mercer
# May 7, 2024

library(tidyverse)

# import  data
deployments <- read.csv("../data_too_big/SNAPSHOT_USA_Deployments.csv") 
observations <- read.csv("../data_too_big/five_year_observation_data.csv") 

deployments$Site_Name <- paste(deployments$Deployment_ID, deployments$Year, sep = "_")

# merge
joined <- left_join(deployments, observations, by = "Site_Name")
joined$Array <- joined$Camera_Trap_Array
joined$Latitude <- joined$Latitude.x
joined$Longitude <- joined$Longitude.x
joined$Survey_Nights <- joined$Survey_Nights.x
joined$Start_Date <- as.Date(joined$Start_Date)
joined$End_Date <- as.Date(joined$End_Date)
joined$Habitat <- joined$Habitat.x
joined$Development_Level <- joined$Development_Level.x
data <- subset(joined, select = c("Array", 
                                  "Site_Name",
                                  "Year",
                                  "Latitude", 
                                  "Longitude", 
                                  "Start_Date",
                                  "End_Date", 
                                  "Survey_Nights",
                                  "Habitat",
                                  "Development_Level",
                                  "Humans_Per_Camera",
                                  "Humans_Per_Camera_Per_Day",
                                  "Disturbance")) 
trimmed <- data %>%
  group_by(Site_Name) %>%
  slice(1) %>%
  ungroup()

deployments <- trimmed

write_csv(deployments, "data/five_year_deployments.csv")
