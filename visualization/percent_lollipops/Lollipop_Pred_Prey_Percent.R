# percent change lollipop chart pred prey
# Margaret Mercer
# June 19, 2024

# clear workspace
rm(list=ls())

library(ggplot2)
library(extrafont)
library(tidyverse)
library(scales)
library(grid)
library(png)

data <- read_csv("results/pred_prey_overlap_results.csv")

data$Pairing <- paste(data$Predator, data$Prey, sep = "/")
data$Pairing <- factor(data$Pairing, levels = rev(unique(data$Pairing)))

# percent change column!
data <- data %>% mutate(Percent_Change = Difference/Overlap_Low)

# Define custom colors
my_colors <- c("Increase" = "#0B5401", "Slight Increase" = "#77A87C", "No Change" = "steelblue", "Slight Decrease" = "#C67976", "Decrease" = "#8B0000", "White" = "white", "Shaded" = "#E5E5E5")

# Create the lollipop chart with legend title removed and custom colors
lol <- ggplot(data, aes(x = Pairing, y = Percent_Change,
                 fill = ifelse(Trend == "increasing", "Increase",
                               ifelse(Trend == "slightly increasing", "Shaded",
                                      ifelse(Trend == "slightly decreasing", "White",
                                             ifelse(Trend == "decreasing", "Decrease", "No Change")))),
                 color = ifelse(Trend == "increasing", "Increase",
                                ifelse(Trend == "slightly increasing", "Increase",
                                       ifelse(Trend == "slightly decreasing", "Decrease",
                                              ifelse(Trend == "decreasing", "Decrease", "No Change"))))
)) +
  geom_segment(aes(xend = Pairing, yend = 0)) +
  geom_rect(data = data[data$Prey_Type == "herbivore", ],
            aes(xmin = as.numeric(Pairing) - 0.5, xmax = as.numeric(Pairing) + 0.5,
                ymin = -Inf, ymax = Inf),
            fill = "#E5E5E5", color = NA) +
  geom_segment(aes(xend = Pairing, yend = 0)) +
  geom_point(shape = 21, size = 3) +
  scale_y_continuous(expand = c(0, 0), limits = c(-.50, .50), labels = percent_format()) +
  coord_flip() +
  theme_classic () +
  theme(axis.title.y = element_blank(),
        panel.border = element_blank(),
        legend.position = "none",
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        text = element_text(family = "Helvetica", size = 15)) +
  geom_hline(yintercept = 0, color = "darkgray") +
  labs(x = NULL, y = "Percent Difference in Temporal Overlap", main = "Percent Difference in Temporal Overlap between Predators and Prey") +
  scale_color_manual(values = my_colors) +  # Set custom colors
  guides(fill = guide_legend(title = NULL), color = guide_legend(title = NULL)) + # Remove legend title
  scale_fill_manual(values = my_colors) +
  geom_text(aes(x = Pairing, y = -.48, label = Prey), hjust = 0, vjust = 0.5, color = "black") 
lol

# add animations
puma <- readPNG("visualization/pngs/puma.png") %>% rasterGrob(interpolate=TRUE)
wolf <- readPNG("visualization/pngs/wolf.png") %>% rasterGrob(interpolate=TRUE)


lol +
  annotation_custom(puma, xmin=11, xmax=17, ymin=0.1, ymax=.5) +
  annotation_custom(wolf, xmin=2, xmax=9, ymin=0.15, ymax=.5)
