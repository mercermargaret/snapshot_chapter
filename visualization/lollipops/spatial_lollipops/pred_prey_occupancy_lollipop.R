# occupancy results pred prey
# margaret mercer
# august 21 2024

# clear workspace
rm(list=ls())

library(ggplot2)
library(extrafont)
library(tidyverse)
library(scales)
library(grid)
library(png)

data <- read_csv("results/pred_prey_occupancy_with_detection_results.csv")

data$Prey_Common <- c("White-tailed Deer",
                      "Mule Deer",
                      "Elk",
                      "Moose",
                      "Coyote",
                      "Bobcat",
                      "Raccoon",
                      "Red Fox",
                      "Striped Skunk",
                      "White-tailed Deer",
                      "Mule Deer",
                      "Elk",
                      "Moose",
                      "Coyote",
                      "Bobcat",
                      "Raccoon",
                      "Red Fox",
                      "Striped Skunk")

data$Pairing <- paste(data$Predator, data$Prey, sep = "/")
data$Pairing <- factor(data$Pairing, levels = rev(unique(data$Pairing)))

# Define custom colors
my_colors <- c("Increase" = "#0B5401", 
               "Slight Increase" = "#77A87C", 
               "No Change" = "steelblue", 
               "Slight Decrease" = "#C67976", 
               "Decrease" = "#8B0000", 
               "White" = "white", 
               "Shaded" = "#E5E5E5")


lol <- ggplot(data, aes(x = Pairing, y = Difference,
                        fill = ifelse(Trend %in% c("slightly increasing", "slightly decreasing"),
                                      ifelse(Prey_Type == "herbivore", "Shaded", "White"),
                                      ifelse(Trend == "increasing", "Increase",
                                             ifelse(Trend == "decreasing", "Decrease", "No Change")))),
              color = ifelse(Trend == "increasing", "Increase",
                             ifelse(Trend == "slightly increasing", "Increase",
                                    ifelse(Trend == "slightly decreasing", "Decrease",
                                           ifelse(Trend == "decreasing", "Decrease", "No Change"))))) +
  geom_segment(aes(xend = Pairing, yend = 0)) +
  geom_rect(data = data[data$Prey_Type == "herbivore", ],
            aes(xmin = as.numeric(Pairing) - 0.5, xmax = as.numeric(Pairing) + 0.5,
                ymin = -Inf, ymax = Inf),
            fill = "#E5E5E5", color = NA) +
  geom_segment(aes(xend = Pairing, yend = 0,
                   color = ifelse(Trend == "increasing", "Increase",
                                  ifelse(Trend == "slightly increasing", "Increase",
                                         ifelse(Trend == "slightly decreasing", "Decrease",
                                                ifelse(Trend == "decreasing", "Decrease", "No Change"))))),
               lwd = 2 ) +
  geom_point(shape = 21, size = 8, stroke = 2,
             aes(color = ifelse(Trend == "increasing", "Increase",
                                ifelse(Trend == "slightly increasing", "Increase",
                                       ifelse(Trend == "slightly decreasing", "Decrease",
                                              ifelse(Trend == "decreasing", "Decrease", "No Change")))))) +
  scale_y_continuous(expand = c(0, 0), limits = c(min(data$Difference) - 4, max(data$Difference) + 4)) +
  coord_flip() +
  theme_classic() +
  theme(axis.title.x = element_text(angle = 0, vjust = 0.5),
        axis.title.y = element_text(size = 15),
        panel.border = element_blank(),
        legend.position = "none",
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        text = element_text(family = "Helvetica", size = 30)) +
  geom_hline(yintercept = 0, color = "darkgray", lwd = 1) +
  labs(x = "Mesocarnivore Prey         Herbivore Prey         Mesocarnivore Prey         Herbivore Prey", 
       y = "Difference in Spatial Overlap", 
       main = "Difference in Spatial Overlap between Predators and Prey") +
  scale_color_manual(values = my_colors) +  # Set custom colors
  scale_fill_manual(values = my_colors) +
  guides(fill = guide_legend(title = NULL), color = guide_legend(title = NULL)) + # Remove legend title
  geom_text(aes(x = Pairing, 
                y = min(Difference) - 3.9, label = Prey_Common), 
            hjust = 0, 
            vjust = 0.5, 
            color = "black", 
            size = 10)
lol

# add animations
puma <- readPNG("visualization/pngs/puma.png") %>% rasterGrob(interpolate=TRUE)
wolf <- readPNG("visualization/pngs/wolf.png") %>% rasterGrob(interpolate=TRUE)


lol +
  annotation_custom(puma, xmin=11, xmax=17, ymin=0, ymax=6) +
  annotation_custom(wolf, xmin=2, xmax=9, ymin=0, ymax=6)

