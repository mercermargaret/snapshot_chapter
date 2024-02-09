# Merging all Years
# Margaret Mercer
# January 24, 2024

library(dplyr)

# import and combine all dataframes
raw2019 <- read.csv("2019.csv")
raw2020 <- read.csv("2020.csv")
raw2021 <- read.csv("2021.csv")
raw2022 <- read.csv("2022.csv")
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
# trim all white space
all_years$Species_Name <- trimws(all_years$Species_Name)
# get rid of any sp, species, Species, Order, Family, family, order (unnecessary qualifiers)
bad <- c(" sp", " species", " Species", " Order", "order", "family", "Family")
regex <- paste(bad, collapse = "|")
all_years$Species_Name <- trimws(sub(regex, "", all_years$Species_Name))
# add unique number for each row
all_years$record_ID <- c(1:965331)
all_years <- select(all_years, ID, Array:Year)
#write csv
write.csv(all_years, "all_years.csv", row.names=FALSE)

# 2019 has 661 dates with missing times
# 2020 has no dates with missing times
# 2021 has 1 date with missing time
# 2022 has 433 dates with missing times
# ugh
# ok it corresponds to the ones where I had to convert the time;
# 2020 and 2021 already had the dates in the correct format so I didn't have to convert.
# So for some reason the 00:00 times are registering correctly for those, but turning into NAs for 2019 and 2022


