# Testing everything on a subset of the data before touching the full dataset
# (partner document is "Preliminary_Data_Messing.R")
# Margaret Mercer
# Dec 14, 2023

obs_2019 <- read.csv("SNAPSHOT_USA_2019_observations.csv")
dep_2019 <- read.csv("SNAPSHOT_USA_2019_deployments.csv")

# merge deployment and observation data
library(dplyr)

head(obs_2019)
head(dep_2019)
# so we want to merge by deployment_ID
# What happens if I...
testdep <- dep_2019[1:5,]
testobs <- obs_2019 |> filter(Deployment_ID %in% c("d58722", "d58723", "d58724", "d58725", "d58726"))
# so I've subset the first 5 deployments of each dataset, just to practice putting them together
# now lets try to merge them...
mergedtest <- merge(testobs, testdep, by = "Deployment_ID", all.x = TRUE)
# hey it worked!!!!
# now let's cut down to which columns we want
all_test <- mergedtest[, c("Deployment_ID", "Site_Name.x", "Survey_Days", "Latitude.x", "Longitude.x", "Begin_Time", "Species_Name", "Count")]


# So now let's try to code a binary for day/night observation
# install.packages("suncalc")
library(suncalc)

# rename columns so suncalc can recognize them
colnames(all_test)[4] <- "lat"
colnames(all_test)[5] <- "lon"
colnames(all_test)[6] <- "date"

#fix date format so suncalc can do its thang

class(all_test$date)

posixct_obj <- as.POSIXct(strptime("8/31/19 6:50", format = "%m/%d/%Y %H:%M")) # with just one date/time, this works!

test_formatted_time <- as.POSIXct(strptime(all_test$date, format = "%m/%d/%Y %H:%M"))

all_test$date <- test_formatted_time

sun_position <- getSunlightPosition(
  data = all_test,
  keep = c("altitude")
)

all_test['Altitude'] = sun_position$altitude

# let's rename some columns so they look nice
colnames(all_test)[2] <- "Site_Name"
colnames(all_test)[4] <- "Latitude"
colnames(all_test)[5] <- "Longitude"
colnames(all_test)[6] <- "Date_Time"

# now let's code 1s and 0s for altitude (sun position) (1 for night observation, 0 for day observation)
# so we want a 1 if it's negative, and a 0 if it's positive
all_test$IsNight <- ifelse(all_test$Altitude < 0, 1, 0)


# ugh let's figure out 2020 data now x_x

testdep20 <- dep_2020[1:5,]
testobs20 <- obs_2020 |> filter(Deployment_ID %in% c("d68315", "d68316", "d68317", "d68318", "d68319"))

mergedtest20 <- merge(testobs20, testdep20, by = "Deployment_ID", all.x = TRUE)

merged20 <- merge(obs_2020, dep_2020, by = "Deployment_ID", all.x = TRUE)
