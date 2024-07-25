# Getting a dataframe of deployments
# Margaret Mercer
# May 7, 2024

library(tidyverse)

# import and wrangle deployment data ####
dep_2019 <- read.csv("data/SNAPSHOT_USA_2019_deployments.csv")
dep_2020 <- read.csv("data/SNAPSHOT_USA_2020_deployments.csv")
dep_2021 <- read.csv("data/ssusa_2021_deployments.csv")
dep_2022 <- read.csv("data/updated_2022_deployments.csv") 
dep_2019$Year <- c(2019)
dep_2020$Year <- c(2020)
dep_2021$Year <- c(2021)
dep_2022$Year <- c(2022)

# add survey days to 2021
Survey_Days <- as.Date(dep_2021$end_date) - as.Date(dep_2021$start_date)
Survey_Days <- as.numeric(gsub("[^0-9]+", "", Survey_Days))
dep_2021$Survey_Days <- Survey_Days

# standardize dates and column names for each year
# 2019
dep_2019$Start_Date <- as.POSIXct(strptime(dep_2019$Date_Out, format = "%m/%d/%y"))
dep_2019$End_Date <- as.POSIXct(strptime(dep_2019$Date_Retrieved, format = "%m/%d/%y"))
dep_2019$Array <- dep_2019$Camera_Trap_Array
dep_2019 <- dep_2019[, c("Array", "Site_Name", "Start_Date", "End_Date", "Survey_Days", "Year", "Latitude", "Longitude")]

# 2020
dep_2020$Start_Date <- as.POSIXct((dep_2020$Date_Out))
dep_2020$End_Date <- as.POSIXct((dep_2020$Date_Retrieved))
dep_2020$Array <- dep_2020$Camera_Trap_Array
dep_2020 <- dep_2020[, c("Array", "Site_Name", "Start_Date", "End_Date", "Survey_Days", "Year", "Latitude", "Longitude")]

# 2021
dep_2021$Start_Date <- as.POSIXct((dep_2021$start_date))
dep_2021$End_Date <- as.POSIXct((dep_2021$end_date))
dep_2021$Array <- dep_2021$subproject_name
dep_2021$Site_Name <- dep_2021$deployment_id
dep_2021$Longitude <- dep_2021$longitude
dep_2021$Latitude <- dep_2021$latitude
dep_2021 <- dep_2021[, c("Array", "Site_Name", "Start_Date", "End_Date", "Survey_Days", "Year", "Latitude", "Longitude")]

# 2022
dep_2022$Start_Date <- as.POSIXct(strptime(dep_2022$start_date, format = "%m/%d/%Y"))
dep_2022$End_Date <- as.POSIXct(strptime(dep_2022$end_date, format = "%m/%d/%Y"))
dep_2022$Array <- dep_2022$subproject_name
dep_2022$Site_Name <- dep_2022$deployment_id
dep_2022$Survey_Days <- dep_2022$survey_days
dep_2022$Longitude <- dep_2022$longitude
dep_2022$Latitude <- dep_2022$latitude
dep_2022 <- dep_2022[, c("Array", "Site_Name", "Start_Date", "End_Date", "Survey_Days", "Year", "Latitude", "Longitude")]


# merge years and trim dataframe
joined <- full_join(dep_2019, dep_2020)
joined <- full_join(joined, dep_2021)
deployments <- full_join(joined, dep_2022)

# add year onto end of site name
deployments$Site_Name <- paste(deployments$Site_Name, deployments$Year, sep = "_")

# filter out the columns for which "survey days" is 0 (cuz they contain no data :'))
deployments <- deployments[deployments$Survey_Days != 0, ]

# import observation data and create encounter histories ####
# let's see if we can merge these in a way that keeps ALL site names (7218) and just has a row of "NA"s if there were no pics.
data_raw <- read.csv("../data_too_big/all_years.csv") 
joined <- left_join(deployments, data_raw, by = "Site_Name")
joined$Array <- joined$Array.x
joined$Year <- joined$Year.x
joined$Latitude <- joined$Latitude.x
joined$Longitude <- joined$Longitude.x
joined$Survey_Days <- joined$Survey_Days.x
joined$Start_Date <- as.Date(joined$Start_Date)
joined$End_Date <- as.Date(joined$End_Date)
data <- subset(joined, select = c("Array", 
                                  "Site_Name",
                                  "Year",
                                  "Latitude", 
                                  "Longitude", 
                                  "Start_Date",
                                  "End_Date", 
                                  "Survey_Days", 
                                  "Humans_Per_Camera_Per_Day")) 
trimmed <- data %>%
  group_by(Site_Name) %>%
  slice(1) %>%
  ungroup()
# cool, so merging like that keeps the 14 rows for which there are NO observations

deployments <- trimmed

write_csv(deployments, "data/deployments.csv")
