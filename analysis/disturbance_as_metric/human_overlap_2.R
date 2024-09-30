# calculate human overlap change in all species for all years as a function of disturbance
# margaret mercer
# sept 27, 2024


# clear workspace
rm(list=ls())

library(tidyverse)
library(overlap)
library(raster)
library(sf)
library(terra)

data <- read_csv("../data_too_big/five_year_observation_data.csv")

data <- data %>%
  mutate(
    Local_Date = as.Date(Local_Date_Time),       # Extract date
    Local_Time = format(Local_Date_Time, "%H:%M:%S")  # Extract time
  )

# filter out NAs for species because we don't care about them
data <- data %>%
  filter(!is.na(Species_Name))

# set species list
species_list <- c("Puma concolor", 
                  "Canis lupus", 
                  "Odocoileus virginianus",
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

range_list <- list(puma,
                wolf,
                whitetail,
                mule,
                elk,
                moose,
                coyote,
                bobcat,
                raccoon,
                fox,
                skunk) 

# create empty dataframe to fill with results
results <- data.frame(
  Species = rep(NA, 11),
  Overlap_Low = rep(NA, 11),
  Overlap_High = rep(NA, 11),
  Difference = rep(NA, 11),
  p_value = rep(NA, 11),
  Median_Human_Dist = rep(NA, 11),
  n_Species_Low = rep(NA, 11),
  n_Species_High = rep(NA, 11),
  n_Human_Low = rep(NA, 11),
  n_Human_High = rep(NA, 11),
  stringsAsFactors = FALSE
)


# for loop time!!
for (i in 1:length(species_list)) {
  
  # pick a species
  species_name <- species_list[i]
  
  # have it tell us what species its working on
  cat("Starting species: ",species_name)
  
  # get range data
  range <- range_list[[i]] # grab a species from range list
  
  st_is_valid(range, reason=TRUE)
  
  # switch off spherical geometry
  sf_use_s2(FALSE)
  
  # Convert df to an sf object
  points_sf <- st_make_valid(st_as_sf(data, coords = c("Longitude", "Latitude"), crs = st_crs(range)))
  
  sf_use_s2(TRUE)
  
  # Perform the point-in-polygon test
  inside <- st_within(points_sf, range, sparse = FALSE)
  
  # Extract rows from df that are inside the polygon
  df_inside <- data[which(inside[,1]),]
  
  # df_inside now contains only the rows where coordinates fall inside the polygon
  
  # # double check that this works by visualizing on map
  # spatial_inside <- st_make_valid(st_as_sf(df_inside, coords = c("Longitude", "Latitude"), crs = st_crs(range)))
  # ggplot() +
  #   geom_sf(data = range[1,], size = 1.5, color = "black", fill = "#690375", alpha = 0.5) +
  #   ggtitle("Species Range") +
  #   geom_sf(data = spatial_inside) +
  #   coord_sf()
  
  
  # Overlap Analysis
  
  species <- filter(df_inside, Species_Name == species_name) # grab a species from species list
  
  # select median of sites and assign to object
  sites <- species %>% 
    group_by(Site_Name) %>%  
    summarize(Disturbance, na.rm = TRUE) %>% 
    unique()
  
  median <- median(sites$Disturbance, na.rm = TRUE)
  
  # filter TOTAL dataframe to low human disturbance
  low_dist <- filter(df_inside, Disturbance < median)
  
  # put time in correct format
  time <- as.POSIXct(low_dist$Local_Time, format = "%H:%M:%S")
  
  # Extract hours, minutes, and seconds
  hours <- as.numeric(format(time, "%H"))
  minutes <- as.numeric(format(time, "%M"))
  seconds <- as.numeric(format(time, "%S"))
  
  # Convert time to radians
  time_radians <- 2 * pi * ((hours + minutes / 60 + seconds / 3600) / 24)
  
  # grab two species of interest out (species and humans)
  low_animal <- time_radians[low_dist$Species_Name == species_name]
  low_human <- time_radians[low_dist$Species_Name == "Homo sapiens" | low_dist$Species_Name == "Vehicle"]
  
  # skip the plotting for now since I'm not sure how to do this in for loop
  # pdf("results/overlap_graphs/puma_human_low.pdf")
  # overlapPlot(low_animal, low_human)
  # legend('topright', c(species, "Human"), lty=c(1,2), col=c(1,4), bty='n')
  # dev.off()
  
  # get overlap for low disturbance
  overlap_low <- overlapEst(low_animal, low_human, type="Dhat4")
  overlap_low
  
  # calculate bootstrap values
  bootstrap_low <- bootstrap(low_animal, low_human, 200, smooth=TRUE, kmax=3, adjust=NA, n.grid=128, type="Dhat4", cores=1)
  # now calculate confidence intervals
  bootCI(overlap_low, bootstrap_low, conf = 0.95)
  
  ## plot overlap for HIGH disturbance
  high_dist <- filter(df_inside, Disturbance >= median)
  
  # convert time into radians
  time <- as.POSIXct(high_dist$Local_Time, format = "%H:%M:%S")
  
  # Extract hours, minutes, and seconds
  hours <- as.numeric(format(time, "%H"))
  minutes <- as.numeric(format(time, "%M"))
  seconds <- as.numeric(format(time, "%S"))
  
  # Convert time to radians
  time_radians <- 2 * pi * ((hours + minutes / 60 + seconds / 3600) / 24)
  
  # plot for high disturbance
  high_animal <- time_radians[high_dist$Species_Name == species_name]
  high_human <- time_radians[high_dist$Species_Name == "Homo sapiens" | high_dist$Species_Name == "Vehicle"]

  # skip the plotting for now since I'm not sure how to do this in for loop
  # pdf("results/overlap_graphs/puma_human_high.pdf")
  # overlapPlot(high_animal, high_human)
  # legend('topright', c("Puma", "Human"), lty=c(1,2), col=c(1,4), bty='n')
  # dev.off()
  
  overlap_high <- overlapEst(high_human, high_animal, type="Dhat4")
  overlap_high
  
  # calculate bootstrap values
  bootstrap_high <- bootstrap(high_human, high_animal, 200, smooth=TRUE, kmax=3, adjust=NA, n.grid=128, type="Dhat4", cores=1)
  # now calculate confidence intervals
  bootCI(overlap_high, bootstrap_high, conf = 0.95)
  
  
  t_test <- t.test(bootstrap_low, bootstrap_high, var.equal = TRUE)
  
  # print results to dataframe
  results[i, 1] <- species_name
  results[i, 2] <- overlap_low
  results[i, 3] <- overlap_high
  results[i, 4] <- (overlap_high - overlap_low)
  results[i, 5] <- t_test$p.value
  results[i, 6] <- median
  results[i, 7] <- length(low_animal)
  results[i, 8] <- length(high_animal)
  results[i, 9] <- length(low_human)
  results[i, 10] <- length(high_human)

}

results$Type <- c("carnivore",
          "carnivore",
          "herbivore",
          "herbivore",
          "herbivore",
          "herbivore",
          "mesocarnivore",
          "mesocarnivore",
          "mesocarnivore",
          "mesocarnivore",
          "mesocarnivore")

results <- results %>%
  mutate(Trend = if_else(Difference < 0, "decreasing", 
                    if_else(Difference > 0, "increasing", "no change")))



write.csv(results, "results/human_overlap_results_2.csv")
