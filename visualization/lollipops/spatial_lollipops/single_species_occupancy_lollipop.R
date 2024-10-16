# occupancy results simple high vs low
# margaret mercer
# august 28 2024

# clear workspace
rm(list=ls())

library(ggplot2)
library(extrafont)
library(tidyverse)
library(scales)
library(grid)
library(png)

data <- read.csv("results/single_species_occupancy_results_3.csv")

data$Species <- factor(data$Species, levels = rev(unique(data$Species)))

# Define custom colors
my_colors <- c("Increase" = "#0B5401", 
               "Slight Increase" = "#77A87C", 
               "No Change" = "steelblue", 
               "Slight Decrease" = "#C67976", 
               "Decrease" = "#8B0000", 
               "White" = "white", 
               "Shaded" = "#E5E5E5")

# Plot
lol <- ggplot(data, aes(x = Species, y = Difference,
                        fill = ifelse(Trend %in% c("slightly increasing", "slightly decreasing"),
                                      ifelse(Type == "herbivore", "Shaded", "White"),
                                      ifelse(Trend == "increasing", "Increase",
                                             ifelse(Trend == "decreasing", "Decrease", "No Change")))),
              color = ifelse(Trend == "increasing", "Increase",
                             ifelse(Trend == "slightly increasing", "Increase",
                                    ifelse(Trend == "slightly decreasing", "Decrease",
                                           ifelse(Trend == "decreasing", "Decrease", "No Change"))))) +
  geom_segment(aes(xend = Species, yend = 0)) +
  geom_rect(data = data[data$Type == "herbivore", ],
            aes(xmin = as.numeric(Species) - 0.5, xmax = as.numeric(Species) + 0.5,
                ymin = -Inf, ymax = Inf),
            fill = "#E5E5E5", color = NA) +
  geom_segment(aes(xend = Species, yend = 0,
               color = ifelse(Trend == "increasing", "Increase",
                              ifelse(Trend == "slightly increasing", "Increase",
                                     ifelse(Trend == "slightly decreasing", "Decrease",
                                            ifelse(Trend == "decreasing", "Decrease", "No Change")))))) +
  geom_point(shape = 21, size = 3, 
             aes(color = ifelse(Trend == "increasing", "Increase",
                                                  ifelse(Trend == "slightly increasing", "Increase",
                                                         ifelse(Trend == "slightly decreasing", "Decrease",
                                                                ifelse(Trend == "decreasing", "Decrease", "No Change")))))) +
  scale_y_continuous(expand = c(0, 0), limits = c(min(data$Difference) - 1.2, max(data$Difference) + .3)) +
  coord_flip() +
  theme_classic() +
  theme(axis.title.y = element_blank(),
        panel.border = element_blank(),
        legend.position = "none",
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        text = element_text(family = "Helvetica", size = 15)) +
  geom_hline(yintercept = 0, color = "darkgray") +
  labs(x = NULL, y = "Difference in Occupancy", title = "Difference in Occupancy Between Low and High Human Presence") +
  scale_color_manual(values = my_colors) +  # Set custom colors
  scale_fill_manual(values = my_colors) +
  guides(fill = guide_legend(title = NULL), color = guide_legend(title = NULL)) + # Remove legend title
  geom_text(aes(x = Species, y = min(Difference) - 1.15, label = Species), hjust = 0, vjust = 0.5, color = "black")
lol


# # add animations
# hum <- readPNG("visualization/pngs/human.png") %>% rasterGrob(interpolate=TRUE)
# 
# lol +
#   annotation_custom(hum, xmin=1, xmax=6, ymin=0.25, ymax=.75)
