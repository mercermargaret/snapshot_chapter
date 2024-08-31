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


# load and format camera trap data
cameras <- read_csv("data/five_year_deployments.csv")
cameras$Year <- as.character(cameras$Year) # do this so when we color the points by year, it doesn't think it's a continuous gradient


trimmed <- cameras %>%
  group_by(Array) %>%
  slice(1) %>%
  ungroup()
trimmed <- trimmed %>%
  distinct_at("Latitude", .keep_all = TRUE)
cameras <- trimmed %>%
  distinct_at("Longitude", .keep_all = TRUE)

points_sf <- st_as_sf(cameras, coords = c("Longitude", "Latitude"),
                      crs = 4326) # make camera locations spatial

# # plot cameras
# ggplot() +
#   geom_sf(data = points_sf, color = "red", size = 1) +
#   theme_void()

# Load the US data from Natural Earth
usa <- ne_states(country = "United States of America", returnclass = "sf")

# get county population info
countyinfo <- read_tsv("../../../Downloads/2022_Gaz_counties_national.txt")
countyinfo$area <- countyinfo$ALAND_SQMI + countyinfo$AWATER_SQMI
countyinfo$fips <- countyinfo$GEOID
countypop <- countypop
new <- left_join(countypop, countyinfo, by = "fips")
new <- dplyr::select(new, GEOID, abbr, county, pop_2022, area)
new$pop_per_area <- new$pop_2022/new$area

new$pop_bin <- factor(cut(new$pop_per_area, 
                                           breaks = c(0, 1, 20, 88, 500, 2000, Inf), 
                                           labels = c("< 1", "1 - 20", "20 - 88", "88 - 500", "500 - 2000", "> 2000")),
                                       levels = c("< 1", "1 - 20", "20 - 88", "88 - 500", "500 - 2000", "> 2000"))


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

bbox <- st_bbox(new_cameras)

# try to get rid of white space?
buffer <- 1000000  # Adjust this buffer as needed
xlim <- c(bbox["xmin"] - buffer, bbox["xmax"] + buffer/3)
ylim <- c(bbox["ymin"] - buffer, bbox["ymax"] + buffer/3)


# plotting camera locations
ggplot() +
  geom_sf(data = new_counties, aes(fill = new_counties_colored$pop_bin), color = NA) +
  geom_sf(data = new_usa, fill = NA, color = "black") +
  geom_sf(data = new_cameras, color = "red", size = 1) +
  theme_void() +
  ggtitle("SnapshotUSA Camera Trap Locations") +
  scale_fill_manual(values = c("< 1" = "#D9D5DA", "1 - 20" = "#B4ABB5", "20 - 88" = "#8E8290",
           "88 - 500" = "#68586B", "500 - 2000" = "#432E46", "> 2000" = "#301934"),
           name = "Humans per Square Mile", na.translate = FALSE) +
  coord_sf(xlim = xlim, ylim = ylim, expand = FALSE) +
  theme(plot.title = element_text(size=22))

# just locations, no pop data
ggplot() +
  geom_sf(data = new_usa, fill = "white", color = "black") +
  geom_sf(data = new_cameras, color = "red", size = 3, alpha = 0.5) +
  theme_void() +
  theme(legend.position = "none") +
  ggtitle("SnapshotUSA Camera Trap Locations") +
  theme(plot.title = element_text(size=22))

