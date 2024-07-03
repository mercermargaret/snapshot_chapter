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

# data$Prey <- factor(data$Prey, levels = rev(unique(data$Prey))) # didn't help with problem when you try to add axis labels

# Define custom colors
my_colors <- c("Increase" = "#0B5401", "Slight Increase" = "#77A87C", "No Change" = "steelblue", "Slight Decrease" = "#C67976", "Decrease" = "#8B0000")

# Create the lollipop chart with legend title removed and custom colors
ggplot(data, aes(x = Pairing, y = Difference,
                 fill = ifelse(Trend == "increasing", "Increase",
                               ifelse(Trend == "slightly increasing", "Slight Increase",
                                      ifelse(Trend == "slightly decreasing", "Slight Decrease",
                                             ifelse(Trend == "decreasing", "Decrease", "No Change")))),
                 color = ifelse(Trend == "increasing", "Increase",
                                ifelse(Trend == "slightly increasing", "Slight Increase",
                                       ifelse(Trend == "slightly decreasing", "Slight Decrease",
                                              ifelse(Trend == "decreasing", "Decrease", "No Change"))))
)) +
  geom_segment(aes(xend = Pairing, yend = 0)) +
  geom_rect(data = data[data$Prey_Type == "herbivore", ],
              aes(xmin = as.numeric(Pairing) - 0.5, xmax = as.numeric(Pairing) + 0.5,
                  ymin = -Inf, ymax = Inf),
              fill = "#E5E5E5", alpha = 0.5, color = NA) +
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
  labs(x = NULL, y = "Difference in Pred/Prey Overlap", main = "Difference of Temporal Overlap Between Predators and Prey") +
  scale_color_manual(values = my_colors) +  # Set custom colors
  guides(fill = guide_legend(title = NULL), color = guide_legend(title = NULL)) + # Remove legend title
  scale_fill_manual(values = my_colors) +
  geom_text(aes(x = Pairing, y = -.29, label = Prey), hjust = 0, vjust = 0.5, color = "black")

