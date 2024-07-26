# Pulling Out Examples of Discrepencies in Snapshot Date Ranges
# they fixed this in updated/merged 5 year version
# Margaret Mercer
# July 24, 2024

dep_2022 <- read.csv("data/updated_2022_deployments.csv") 
obs_2022 <- read.csv("data/updated_2022_sequences.csv") 

# example
ex1dep <- dep_2022[which(dep_2022$deployment_id == "AK_Forest_Crupi_22_loc02 08/09/2022"),] 
# start date is August 9
ex1obs <- obs_2022[which(obs_2022$deployment_id == "AK_Forest_Crupi_22_loc02 08/09/2022"),] 
# first three observations took place August 8


# example 2
ex2dep <- dep_2022[which(dep_2022$deployment_id == "AR_Forest_Johans_22_14.2"),] 
# start date is Oct 6
ex2obs <- obs_2022[which(obs_2022$deployment_id == "AR_Forest_Johans_22_14.2"),] 
# many many observations took place before Oct 6 (beginning August 23)


# import and merge dataset with all years
all_deployments <- read.csv("data/deployments.csv") 
all_observations <- read.csv("../data_too_big/all_years.csv") 
joined <- left_join(all_deployments, all_observations, by = "Site_Name")
joined$Array <- joined$Array.x
joined$Year <- joined$Year.x
joined$Latitude <- joined$Latitude.x
joined$Longitude <- joined$Longitude.x
joined$Survey_Days <- joined$Survey_Days.x
all_data <- subset(joined, select = c("record_ID", 
                                  "Array", 
                                  "Site_Name", 
                                  "Survey_Days", 
                                  "Latitude", 
                                  "Longitude", 
                                  "Local_Date", 
                                  "Local_Time", 
                                  "Species_Name", 
                                  "Time_Zone",
                                  "UTC_Date_Time",
                                  "Year",
                                  "Start_Date",
                                  "End_Date")) 

# all rows for 2022 for which photograph date is outside date range
observations_outside_date_range <- all_data[which(as.Date(all_data$Local_Date) > as.Date(all_data$End_Date) |
                                                as.Date(all_data$Local_Date) < as.Date(all_data$Start_Date)), ]
