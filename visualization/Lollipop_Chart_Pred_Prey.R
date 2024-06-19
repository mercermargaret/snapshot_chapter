# lollipop chart for overlap of pred vs prey
# Margaret Mercer
# May 8, 2024

library(ggplot2)
library(extrafont)
library(tidyverse)
library(scales)

data <- read_csv("data/Pred_Prey_Overlap_Results - Cutoff at pairing median.csv")

data$Pairing <- paste(data$Predator, data$Prey, sep = "/")

data$Pairing <- factor(data$Pairing, levels = rev(unique(data$Pairing)))

# Define custom colors
my_colors <- c("Increase" = "darkgreen", "No Change" = "steelblue", "Decrease" = "darkred")

# Create the lollipop chart with legend title removed and custom colors
ggplot(data, aes(x = Pairing, y = Difference,
                 fill = ifelse(Difference > 0, "Increase", ifelse(Difference < 0, "Decrease", "No Change")),
                 color = ifelse(Difference > 0, "Increase", ifelse(Difference < 0, "Decrease", "No Change"))
)) +
  # geom_rect(data = data[data$Prey_Type == "herbivore", ],
  #           aes(xmin = as.numeric(Pairing) - 0.5, xmax = as.numeric(Pairing) + 0.5,
  #               ymin = -Inf, ymax = Inf),
  #           fill = "gray", alpha = 0.5, color = NA) + 
  # for some reason, when "geom_rect" is added it messes with the order of the lollipops and puts the y axis in alphabetical order :')
  geom_segment(aes(xend = Pairing, yend = 0)) +
  geom_point(shape = 21, size = 3) +
  scale_y_continuous(expand = c(0, 0), limits = c(-.30, .30), labels = percent_format()) +
  coord_flip() +
  theme_classic () +
  theme(axis.title.y = element_blank(),
        panel.border = element_blank(),
        legend.position = "none",
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        text = element_text(family = "Helvetica", size = 15)) +
  geom_hline(yintercept = 0, color = "darkgray") +
  labs(x = NULL, y = "Difference in Temporal Overlap", main = "Difference in Temporal Overlap between Predators and Prey in Low versus High Human activity") +
  scale_color_manual(values = my_colors) +  # Set custom colors
  guides(fill = guide_legend(title = NULL), color = guide_legend(title = NULL)) + # Remove legend title
  scale_fill_manual(values = my_colors)

