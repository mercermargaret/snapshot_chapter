# Visualizing Camera Locations
# Margaret Mercer
# May 1, 2024

library(tidyverse)
library(ggplot2)
library(sf)
library(rnaturalearthdata)
library(rnaturalearth)
library(tigris)
library(grid)
library(usmap)
library(maps)

# the code I actually used is at the bottom of the document; everything else is just figuring stuff out


# # load and format camera trap data
# cameras <- read_csv("data/cameras.csv")
# cameras$Year <- as.character(cameras$Year) # do this so when we color the points by year, it doesn't think it's a continuous gradient
# points_sf <- st_as_sf(cameras, coords = c("Longitude", "Latitude"),
#                        crs = 4326) # make camera locations spatial
# 
# # Load states data from Natural Earth
# usa <- ne_states(country = "united states of america", returnclass = "sf")
# 
# 
# # Move Alaska and Hawaii below the contiguous US while keeping alaska to scale
# # you can have "position" set to "below" or "outside" BUT you have to change BOTH line 34 AND line 41! I personally like "below" just a bit better
# new_usa <- tigris::shift_geometry(
#   usa,
#   geoid_column = NULL,
#   preserve_area = TRUE,
#   position = "below"
# )
# 
# new_cameras <- tigris::shift_geometry(
#   points_sf,
#   geoid_column = NULL,
#   preserve_area = TRUE,
#   position = "below"
# )
# 
# # plotting camera locations
# ggplot() +
#   geom_sf(data = new_usa, fill = "white", color = "black") +
#   geom_sf(data = new_cameras, color = "red", size = 1) +
#   # geom_sf(data = new, color = "pop_per_area") +
#   theme_void() +
#   theme(legend.position = "none") +
#   ggtitle("Camera Trap Locations")
# 
# # # the following is code to create an inset zooming in on a little piece of the map to show an array. Upon further consideration, I don't think it's a priority
# # # subset map
# # mini <- ggplot(data = usa) +
# #   geom_sf(data = new_usa, fill = "white", color = "black") +
# #   geom_sf(data = new_cameras, color = "blue", size = 2) +
# #   coord_sf(xlim = c(-124.2075, -124.185), ylim = c(40.692, 40.705), expand = FALSE) +
# #   theme_bw() +
# #   theme(legend.position = "none",
# #         axis.text.y = element_blank(),
# #         axis.text.x = element_blank(),
# #         axis.ticks = element_blank())
# # mini
# # 
# # # make it a grob
# # mini_grob <- ggplotGrob(mini)
# # 
# # map + 
# #   annotation_custom(grob = mini_grob, 
# #                     xmin = -120, xmax = -120, 
# #                     ymin = 38, ymax = 38)
# # 
# # # put them together
# # ggplot() +
# #   coord_equal(xlim = c(0, 28), ylim = c(0, 20), expand = FALSE) +
# #   annotation_custom(ggplotGrob(map), xmin = 0, xmax = 20, ymin = 0, ymax = 20) +
# #   annotation_custom(ggplotGrob(mini), xmin = 20, xmax = 28, ymin = 11.25, ymax = 19) +
# #   theme_void()
# 
# 
# 
# # color locations  by year
# # (not sure if I care about this)
# # ggplot(data = new_cameras, aes(color = Year)) +
# #   geom_sf(data = new_usa, fill = "white", color = "black") +
# #   geom_sf(size = 1) +
# #   theme_bw() +
# #   ggtitle("Camera Trap Locations By Year")
# 
# 
# 
# # # map humans high vs humans low
# # Not much of a visual difference here because many of them are within the same array
# # # low disturbance
# # low <- filter(cameras, Humans_Per_Camera_Per_Day < median(cameras$Humans_Per_Camera_Per_Day))
# # 
# # low$Year <- as.character(low$Year) # do this so when we color the points by year, it doesn't think it's a continuous gradient
# # low_sf <- st_as_sf(low, coords = c("Longitude", "Latitude"),
# #                       crs = 4326)
# # 
# # new_low <- tigris::shift_geometry(
# #   low_sf,
# #   geoid_column = NULL,
# #   preserve_area = TRUE,
# #   position = "below"
# # )
# # 
# # 
# # ggplot(data = new_low, aes(color = Year)) +
# #   geom_sf(data = new_usa, fill = "white", color = "black") +
# #   geom_sf(size = 1) +
# #   theme_void() +
# #   ggtitle("Low Human Disturbance: Camera Trap Locations By Year")
# # 
# # 
# # # high disturbance
# # high <- filter(cameras, Humans_Per_Camera_Per_Day >= median(cameras$Humans_Per_Camera_Per_Day))
# # 
# # high$Year <- as.character(high$Year) # do this so when we color the points by year, it doesn't think it's a continuous gradient
# # high_sf <- st_as_sf(high, coords = c("Longitude", "Latitude"),
# #                    crs = 4326)
# # 
# # new_high <- tigris::shift_geometry(
# #   high_sf,
# #   geoid_column = NULL,
# #   preserve_area = TRUE,
# #   position = "below"
# # )
# # 
# # ggplot(data = new_high, aes(color = Year)) +
# #   geom_sf(data = new_usa, fill = "white", color = "black") +
# #   geom_sf(size = 1) +
# #   theme_void() +
# #   ggtitle("High Human Disturbance: Camera Trap Locations By Year")
# 
# 
# 
# 
# # lets plot county populations
# # 
# # plot_usmap(regions = "counties") + 
# #   labs(title = "U.S. counties",
# #        subtitle = "This is a blank map of the United States.") + 
# #   theme(panel.background=element_blank())
# # 
# # plot_usmap(data = countypop, values = "pop_2022", color = NA) + 
# #   scale_fill_continuous(low = "white", high = "red", name = "Population", label = scales::comma) + 
# #   labs(title = "New England Region", subtitle = "Population in New England Counties in 2022") +
# #   theme(legend.justification = "right")
# # 
# # # get population per square mile
# countyinfo <- read_tsv("../../../Downloads/2022_Gaz_counties_national.txt")
# countyinfo$area <- countyinfo$ALAND_SQMI + countyinfo$AWATER_SQMI
# countyinfo$fips <- countyinfo$GEOID
# countypop <- countypop
# new <- left_join(countypop, countyinfo, by = "fips")
# new <- select(new, fips, GEOID, abbr, county, pop_2022, area)
# new$pop_per_area <- new$pop_2022/new$area
# 
# # plot_usmap(data = new, values = "pop_per_area")
# 
# 
# 
# # I don't know why this doesn't work. It's getting stuck when I try to color counties by the "color" column (line 205)
# # Define the bins and corresponding colors
# bins <- c(0, 1, 20, 88, 500, 2000, Inf)
# colors <- c("#D9D5DA", "#B4ABB5", "#8E8290", "#68586B", "#432E46", "#301934")
# 
# # Create a function to assign colors based on the bins
# assign_color <- function(value) {
#   color <- colors[findInterval(value, bins)]
#   return(color)
# }
# 
# # Apply color assignment function to your data
# new$color <- sapply(new$pop_per_area, assign_color)
# # color_column <- new$color
# # pops <- new$pop_per_area
# 
# # # Plot the US map with county populations colored according to the specified bins
# # plot_usmap(data = new, values = "color", color = NA) +
# #   scale_fill_identity() +  # Ensure the legend matches the specified colors
# #   labs(title = "County Populations by Area", fill = "Population") +
# #   theme(legend.position = "right") +  # Adjust legend position
# #   guides(fill = guide_legend(title = "Population Bins", override.aes = list(color = colors))) +  # Customize legend title and colors
# #   geom_sf(data = new_usa, fill = NA, color = "black") +
# #   geom_sf(data = new_cameras, color = "red", size = 1) +
# #   theme_void() +
# #   theme(legend.position = "none") +
# #   ggtitle("Camera Trap Locations")
# # this is cool but alaska and hawaii are not lined up in the two maps I'm overlaying
# 
# # lets fix alaska and hawaii
# # WHAT IF i do this manually
# # first I have to cut alaska and hawaii out of the "new" dataframe
# test_new <- filter(new, abbr != "AK" & abbr != "HI")
# 
# # then I have to manually color in alaska and hawaii in the "new_usa" dataframe
# # first I'm gonna split up the continental US from alaska and hawaii
# continental <- new_usa %>% filter(name != "Alaska" & name != "Hawaii")
# ak <- new_usa %>% filter(name == "Alaska")
# hi <- new_usa %>% filter(name == "Hawaii")
# 
# # Then I'm gonna plot the US and then put alaska and hawaii on there too and color them in
# plot_usmap(data = test_new, values = "color", color = NA) +
#   scale_fill_identity() +  # Ensure the legend matches the specified colors
#   labs(title = "County Populations by Area", fill = "Population") +
#   theme(legend.position = "right") +  # Adjust legend position
#   guides(fill = guide_legend(title = "Population Bins", override.aes = list(color = colors))) +  # Customize legend title and colors
#   geom_sf(data = continental, fill = NA, color = "black") +
#   geom_sf(data = ak, fill = "#D9D5DA", color = "black") +
#   geom_sf(data = hi, fill = "#8E8290", color = "black") +
#   geom_sf(data = new_cameras, color = "red", size = 1) +
#   theme_void() +
#   theme(legend.position = "none") +
#   ggtitle("Camera Trap Locations")
# 
# 
# # now let's try a new approach -- getting county information for ak and hi dataframes
# 
# 
# 
# # Load the Alaska state data from Natural Earth
# alaska_state <- ne_states(country = "United States of America", returnclass = "sf") %>%
#   dplyr::filter(name == "Alaska")
# 
# # Load the Alaska county data from TIGER/Line
# alaska_counties <- counties(state = "AK", cb = TRUE, class = "sf")
# ak_counties_colored <- left_join(alaska_counties, new, by = "GEOID")
# 
# # Define the plot
# ggplot() +
#   # Plot Alaska state outline
#   geom_sf(data = alaska_state, fill = NA, color = "black") +
#   # Plot Alaska counties
#   geom_sf(data = alaska_counties, fill = ak_counties_colored$color, color = NA) +
#   coord_sf() +
#   theme_void() +
#   labs(title = "Alaska with County Boundaries")
# 
# # IT WORKED!! FUCKKKK YESSSSS
# 




# HOLD up I think I can do this with the whole country and skip all the plot_usmap bullshit??

# START OVER HERE
# load and format camera trap data
cameras <- read_csv("data/cameras.csv")
cameras$Year <- as.character(cameras$Year) # do this so when we color the points by year, it doesn't think it's a continuous gradient
points_sf <- st_as_sf(cameras, coords = c("Longitude", "Latitude"),
                      crs = 4326) # make camera locations spatial

# # plot cameras
# ggplot() +
#   geom_sf(data = points_sf, color = "red", size = 1) +
#   theme_void()

# Load the US data from Natural Earth
usa <- ne_states(country = "United States of America", returnclass = "sf")

# # plot ne data and cameras
# ggplot() +
#   geom_sf(data = usa, fill = "white", color = "black") +
#   geom_sf(data = points_sf, color = "red", size = 1) +
#   theme_void() +
#   theme(legend.position = "none") +
#   ggtitle("Camera Trap Locations")

# get county population info
countyinfo <- read_tsv("../../../Downloads/2022_Gaz_counties_national.txt")
countyinfo$area <- countyinfo$ALAND_SQMI + countyinfo$AWATER_SQMI
countyinfo$fips <- countyinfo$GEOID
countypop <- countypop
new <- left_join(countypop, countyinfo, by = "fips")
new <- select(new, GEOID, abbr, county, pop_2022, area)
new$pop_per_area <- new$pop_2022/new$area


# assign colors to counties
# Define the bins and corresponding colors
bins <- c(0, 1, 20, 88, 500, 2000, Inf)
colors <- c("#D9D5DA", "#B4ABB5", "#8E8290", "#68586B", "#432E46", "#301934")

# Create a function to assign colors based on the bins
assign_color <- function(value) {
  color <- colors[findInterval(value, bins)]
  return(color)
}

# Apply color assignment function to your data
new$color <- sapply(new$pop_per_area, assign_color)

# Load the county data from TIGER/Line
us_counties <- counties(cb = TRUE, class = "sf")
us_counties <- us_counties %>% filter(STATE_NAME != "Puerto Rico") # get rid of PR (RIP)
counties_colored <- left_join(us_counties, new, by = "GEOID")




# MOVE alaska and hawaii in ALL layers
new_usa <- tigris::shift_geometry(
  usa,
  geoid_column = NULL,
  preserve_area = TRUE,
  position = "below"
)

new_cameras <- tigris::shift_geometry(
  points_sf,
  geoid_column = NULL,
  preserve_area = TRUE,
  position = "below"
)

new_counties <- tigris::shift_geometry(
  us_counties,
  geoid_column = NULL,
  preserve_area = TRUE,
  position = "below"
)

new_counties_colored <- tigris::shift_geometry(
  counties_colored,
  geoid_column = NULL,
  preserve_area = TRUE,
  position = "below"
)


# plotting camera locations
ggplot() +
  geom_sf(data = new_counties, fill = new_counties_colored$color, color = NA) +
  geom_sf(data = new_usa, fill = NA, color = "black") +
  geom_sf(data = new_cameras, color = "red", size = 1) +
  theme_void() +
  theme(legend.position = "none") +
  ggtitle("Camera Trap Locations")
