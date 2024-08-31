# lollipop chart for overlap of pred vs prey
# Margaret Mercer
# May 8, 2024

library(ggplot2)
library(extrafont)
library(tidyverse)
library(scales)
library(grid)
library(png)

data <- read_csv("results/pred_prey_overlap_results.csv")

data$Pairing <- paste(data$Predator, data$Prey, sep = "/")
data$Pairing <- factor(data$Pairing, levels = rev(unique(data$Pairing)))

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

# Define custom colors
my_colors <- c("Increase" = "#0B5401",
               "Slight Increase" = "#77A87C",
               "No Change" = "steelblue",
               "Slight Decrease" = "#C67976",
               "Decrease" = "#8B0000",
               "White" = "white",
               "Shaded" = "#E5E5E5")



# add animations
puma <- readPNG("visualization/pngs/puma.png") %>% rasterGrob(interpolate=TRUE)
wolf <- readPNG("visualization/pngs/wolf.png") %>% rasterGrob(interpolate=TRUE)


# full lollipop chart ####
lol <- ggplot(data, aes(x = Pairing, y = Difference,
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
  scale_y_continuous(expand = c(0, 0), limits = c(min(data$Difference) - 0.09, max(data$Difference) + .05), labels = percent_format()) +
  coord_flip() +
  theme_classic () +
  theme(axis.title.y = element_blank(),
        panel.border = element_blank(),
        legend.position = "none",
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        text = element_text(family = "Helvetica", size = 15)) +
  geom_hline(yintercept = 0, color = "darkgray") +
  labs(x = NULL, y = "Difference in Temporal Overlap", main = "Difference in Temporal Overlap between Predators and Prey") +
  scale_color_manual(values = my_colors) +  # Set custom colors
  guides(fill = guide_legend(title = NULL), color = guide_legend(title = NULL)) + # Remove legend title
  scale_fill_manual(values = my_colors) +
  geom_text(aes(x = Pairing, y = min(Difference) - 0.08, label = Prey_Common), hjust = 0, vjust = 0.5, color = "black") 
lol


lol +
  annotation_custom(puma, xmin=11, xmax=17, ymin=0.08, ymax=.23) +
  annotation_custom(wolf, xmin=2, xmax=9, ymin=0.13, ymax=.23)

# empty chart ####
lol_empty <- ggplot(data, aes(x = Pairing, y = Difference,
                              fill = "White",
                              color = "White")) +
  geom_segment(aes(xend = Pairing, yend = 0)) +
  geom_point(shape = 21, size = 3) +
  scale_y_continuous(expand = c(0, 0), limits = c(min(data$Difference) - 0.09, max(data$Difference) + .05), labels = percent_format()) +
  coord_flip() +
  theme_classic () +
  theme(axis.title.y = element_blank(),
        panel.border = element_blank(),
        legend.position = "none",
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        text = element_text(family = "Helvetica", size = 15)) +
  geom_hline(yintercept = 0, color = "darkgray") +
  labs(x = NULL, y = "Difference in Temporal Overlap", main = "Difference in Temporal Overlap between Predators and Prey") +
  scale_color_manual(values = my_colors) +  # Set custom colors
  guides(fill = guide_legend(title = NULL), color = guide_legend(title = NULL)) + # Remove legend title
  scale_fill_manual(values = my_colors) +
  geom_text(aes(x = Pairing, y = min(Difference) - 0.08, label = NA), hjust = 0, vjust = 0.5, color = "black") 
lol_empty



# add prey labs ####
lol_lab <- ggplot(data, aes(x = Pairing, y = Difference,
                            fill = "White",
                            color = "White")) +
  geom_segment(aes(xend = Pairing, yend = 0)) +
  geom_point(shape = 21, size = 3) +
  scale_y_continuous(expand = c(0, 0), limits = c(min(data$Difference) - 0.09, max(data$Difference) + .05), labels = percent_format()) +
  coord_flip() +
  theme_classic () +
  theme(axis.title.y = element_blank(),
        panel.border = element_blank(),
        legend.position = "none",
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        text = element_text(family = "Helvetica", size = 15)) +
  geom_hline(yintercept = 0, color = "darkgray") +
  labs(x = NULL, y = "Difference in Temporal Overlap", main = "Difference in Temporal Overlap between Predators and Prey") +
  scale_color_manual(values = my_colors) +  # Set custom colors
  guides(fill = guide_legend(title = NULL), color = guide_legend(title = NULL)) + # Remove legend title
  scale_fill_manual(values = my_colors) +
  geom_text(aes(x = Pairing, y = min(Difference) - 0.08, label = Prey_Common), hjust = 0, vjust = 0.5, color = "black") 
lol_lab

lol_lab +
  annotation_custom(puma, xmin=11, xmax=17, ymin=0.08, ymax=.23) +
  annotation_custom(wolf, xmin=2, xmax=9, ymin=0.13, ymax=.23)

# add boxes ####
lol_box <- ggplot(data, aes(x = Pairing, y = Difference,
                            fill = "White",
                            color = "White")) +
  geom_segment(aes(xend = Pairing, yend = 0)) +
  geom_rect(data = data[data$Prey_Type == "herbivore", ],
            aes(xmin = as.numeric(Pairing) - 0.5, xmax = as.numeric(Pairing) + 0.5,
                ymin = -Inf, ymax = Inf),
            fill = "#E5E5E5", color = NA) +
  scale_y_continuous(expand = c(0, 0), limits = c(min(data$Difference) - 0.09, max(data$Difference) + .05), labels = percent_format()) +
  coord_flip() +
  theme_classic () +
  theme(axis.title.y = element_blank(),
        panel.border = element_blank(),
        legend.position = "none",
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        text = element_text(family = "Helvetica", size = 15)) +
  geom_hline(yintercept = 0, color = "darkgray") +
  labs(x = NULL, y = "Difference in Temporal Overlap", main = "Difference in Temporal Overlap between Predators and Prey") +
  scale_color_manual(values = my_colors) +  # Set custom colors
  guides(fill = guide_legend(title = NULL), color = guide_legend(title = NULL)) + # Remove legend title
  scale_fill_manual(values = my_colors) +
  geom_text(aes(x = Pairing, y = min(Difference) - 0.08, label = Prey_Common), hjust = 0, vjust = 0.5, color = "black")
lol_box

lol_box +
  annotation_custom(puma, xmin=11, xmax=17, ymin=0.08, ymax=.23) +
  annotation_custom(wolf, xmin=2, xmax=9, ymin=0.13, ymax=.23)

# puma prey ####
lol_pum <- ggplot(data, aes(x = Pairing, y = Difference,
                               fill = ifelse(Trend == "increasing", "Increase",
                                             ifelse(Trend == "slightly increasing", "Shaded",
                                                    ifelse(Trend == "slightly decreasing", "White",
                                                           ifelse(Trend == "decreasing", "Decrease", "No Change")))),
                               color = ifelse(Trend == "increasing", "Increase",
                                              ifelse(Trend == "slightly increasing", "Increase",
                                                     ifelse(Trend == "slightly decreasing", "Decrease",
                                                            ifelse(Trend == "decreasing", "Decrease", "No Change"))))
)) +
  geom_segment(aes(xend = Pairing, yend = 0), 
               alpha = ifelse(data$Predator == "Puma concolor", 1, 0)) +
  geom_rect(data = data[data$Prey_Type == "herbivore", ],
            aes(xmin = as.numeric(Pairing) - 0.5, xmax = as.numeric(Pairing) + 0.5,
                ymin = -Inf, ymax = Inf),
            fill = "#E5E5E5", color = NA) +
  geom_segment(aes(xend = Pairing, yend = 0), 
               alpha = ifelse(data$Predator == "Puma concolor", 1, 0)) +
  geom_point(shape = 21, size = 3, 
             alpha = ifelse(data$Predator == "Puma concolor", 1, 0)) +
  scale_y_continuous(expand = c(0, 0), limits = c(min(data$Difference) - 0.09, max(data$Difference) + .05), labels = percent_format()) +
  coord_flip() +
  theme_classic () +
  theme(axis.title.y = element_blank(),
        panel.border = element_blank(),
        legend.position = "none",
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        text = element_text(family = "Helvetica", size = 15)) +
  geom_hline(yintercept = 0, color = "darkgray") +
  labs(x = NULL, y = "Difference in Temporal Overlap", main = "Difference in Temporal Overlap between Predators and Prey") +
  scale_color_manual(values = my_colors) +  # Set custom colors
  guides(fill = guide_legend(title = NULL), color = guide_legend(title = NULL)) + # Remove legend title
  scale_fill_manual(values = my_colors) +
  geom_text(aes(x = Pairing, y = min(Difference) - 0.08, label = Prey_Common), hjust = 0, vjust = 0.5, color = "black") 
lol_pum

lol_pum +
  annotation_custom(puma, xmin=11, xmax=17, ymin=0.08, ymax=.23) +
  annotation_custom(wolf, xmin=2, xmax=9, ymin=0.13, ymax=.23)
