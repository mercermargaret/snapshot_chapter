# Preliminary Data Messing
# Margaret Mercer
# Dec 11, 2023

obs_2019 <- read.csv("SNAPSHOT_USA_2019_observations.csv")
dep_2019 <- read.csv("SNAPSHOT_USA_2019_deployments.csv")
obs_2020 <- read.csv("SNAPSHOT_USA_2020_observations.csv")
dep_2020 <- read.csv("SNAPSHOT_USA_2020_deployments.csv")

# merge deployment and observation data
library(dplyr)

left_joined <- merge(obs_2019, dep_2019, by = "Deployment_ID", all.x = TRUE)
all_2019 <- left_joined[, c("Deployment_ID", "Site_Name.x", "Survey_Days", "Latitude.x", "Longitude.x", "Begin_Time", "Time_Zone", "Species_Name", "Count")]
# Ok now the merged has the right amount of observations, and we've included only the columns we want. Nice.

# So now let's try to code a binary for day/night observation
# install.packages("suncalc")
library(suncalc)

# rename columns so suncalc can recognize them
colnames(all_2019)[4] <- "lat"
colnames(all_2019)[5] <- "lon"
colnames(all_2019)[6] <- "date"

#fix date format so suncalc can do its thang
formatted_time <- as.POSIXct(strptime(all_2019$date, format = "%m/%d/%Y %H:%M"))

all_2019$date <- formatted_time

sun_position <- getSunlightPosition(
  data = all_2019,
  keep = c("altitude")
)

all_2019['Altitude'] = sun_position$altitude

# let's rename some columns so they look nice
colnames(all_2019)[2] <- "Site_Name"
colnames(all_2019)[4] <- "Latitude"
colnames(all_2019)[5] <- "Longitude"
colnames(all_2019)[6] <- "Date_Time"

# now let's code 1s and 0s for altitude (sun position) (1 for night observation, 0 for day observation)
# so we want a 1 if it's negative, and a 0 if it's positive
all_2019$IsNight <- ifelse(all_2019$Altitude < 0, 1, 0)

