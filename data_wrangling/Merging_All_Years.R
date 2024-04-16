# Merging all Years
# Margaret Mercer
# January 24, 2024

library(tidyr)
library(dplyr)

# import and combine all dataframes
raw2019 <- read.csv("data/2019.csv")
raw2020 <- read.csv("data/2020.csv")
raw2021 <- read.csv("data/2021.csv")
raw2022 <- read.csv("data/2022.csv")
raw2019$Year <- c(2019)
raw2020$Year <- c(2020)
raw2021$Year <- c(2021)
raw2022$Year <- c(2022)
joined <- full_join(raw2019, raw2020)
joined <- full_join(joined, raw2021)
all_years <- full_join(joined, raw2022)

# replace all humans with "Homo sapiens"
all_years$Species_Name <- replace(all_years$Species_Name, all_years$Species_Name == "Camera Trapper" | all_years$Species_Name == "Crew Member" | all_years$Species_Name == "Human non-staff", "Homo sapiens")
# replace all vehicles with "Vehicle"
all_years$Species_Name <- replace(all_years$Species_Name, all_years$Species_Name == "Motor vehicle" | all_years$Species_Name == "ATV" | all_years$Species_Name == "Motorcycle" | all_years$Species_Name == "Official Vehicle" | all_years$Species_Name == "Truck" | all_years$Species_Name == "Vehicle-Cart", "Vehicle")
# standardize bear name
all_years$Species_Name <- replace(all_years$Species_Name, all_years$Species_Name == "Ursus U. arctos", "Ursus arctos")
# trim all white space
all_years$Species_Name <- trimws(all_years$Species_Name)
# get rid of any sp, species, Species, Order, Family, family, order (unnecessary qualifiers)
bad <- c(" sp", " species", " Species", " Order", "order", "family", "Family")
regex <- paste(bad, collapse = "|")
all_years$Species_Name <- trimws(sub(regex, "", all_years$Species_Name))
# add unique number for each row
all_years$record_ID <- c(1:965331)
all_years <- dplyr::select(all_years, record_ID, Array:Year)
# change Survey_Days "0"s to "1" (it was out a part of a day, not really ZERO days)
all_years$Survey_Days <- replace(all_years$Survey_Days, all_years$Survey_Days == 0, 1)

all_years$Human <- ifelse(all_years$Species_Name == "Homo sapiens" | all_years$Species_Name == "Vehicle", 1, 0)
Humans_Per_Camera <- all_years %>% filter(Human == 1) %>%
  group_by(Site_Name) %>%
  summarise(Humans_Per_Camera = n())
all_years <- left_join(all_years, Humans_Per_Camera, by = "Site_Name") %>%
  mutate(Humans_Per_Camera = tidyr::replace_na(Humans_Per_Camera, 0)) # replace nas with 0
all_years$Humans_Per_Camera_Per_Day <- all_years$Humans_Per_Camera/all_years$Survey_Days
all_years <- subset(all_years, select = c(-Human))

# potential other changes: change "rabbit or hare" to "Leporidae"

# get difference between time and noon of the same day
all_years <- separate(all_years, Local_Date_Time, c("Local_Date", "Local_Time"), sep = " ")

all_years$Local_Date_Time <- as.POSIXct(paste(all_years$Local_Date, all_years$Local_Time), format="%Y-%m-%d %H:%M:%S")
all_years$noon <- as.POSIXct(paste(all_years$Local_Date, "12:00:00"), format="%Y-%m-%d %H:%M:%S")

all_years$Difference_From_Noon <- as.numeric(difftime(all_years$Local_Date_Time, all_years$noon, units = "mins"))

all_years <- subset(all_years, select = c(-noon, -Local_Date_Time))

# write csv
write.csv(all_years, "data/all_years.csv", row.names=FALSE)

