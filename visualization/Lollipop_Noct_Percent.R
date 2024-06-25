# lollipop chart for noct percentage
# Margaret Mercer
# June 25, 2024

library(ggplot2)
library(extrafont)
library(tidyverse)
library(scales)

data <- read_csv("data/Nocturnality_Results - USE THIS ONE.csv")

data <- data[c(1:2, 5:9, 14, 16:18), ]

data$Species <- factor(data$Species, levels = rev(unique(data$Species)))

# percent change column!
data <- data %>% mutate(Percent_Change = Noct_Diff/Noct_Low)

# Define custom colors
my_colors <- c("Increase" = "#0B5401", "Slight Increase" = "#77A87C", "No Change" = "steelblue", "Slight Decrease" = "#C67976", "Decrease" = "#8B0000")

# Create the lollipop chart with legend title removed and custom colors
ggplot(data, aes(x = Species, y = Percent_Change,
                 fill = ifelse(Trend == "increasing", "Increase",
                               ifelse(Trend == "slightly increasing", "Slight Increase",
                                      ifelse(Trend == "slightly decreasing", "Slight Decrease",
                                             ifelse(Trend == "decreasing", "Decrease", "No Change")))),
                 color = ifelse(Trend == "increasing", "Increase",
                                ifelse(Trend == "slightly increasing", "Slight Increase",
                                       ifelse(Trend == "slightly decreasing", "Slight Decrease",
                                              ifelse(Trend == "decreasing", "Decrease", "No Change"))))
)) +
  geom_segment(aes(xend = Species, yend = 0)) +
  geom_rect(data = data[data$Type == "herbivore", ],
            aes(xmin = as.numeric(Species) - 0.5, xmax = as.numeric(Species) + 0.5,
                ymin = -Inf, ymax = Inf),
            fill = "gray", alpha = 0.5, color = NA) +
  geom_segment(aes(xend = Species, yend = 0)) +
  geom_point(shape = 21, size = 3) +
  scale_y_continuous(expand = c(0, 0), limits = c(-.35, .35), labels = percent_format()) +
  coord_flip() +
  theme_classic () +
  theme(axis.title.y = element_blank(),
        panel.border = element_blank(),
        legend.position = "none",
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        text = element_text(family = "Helvetica", size = 15)) +
  geom_hline(yintercept = 0, color = "darkgray") +
  labs(x = NULL, y = "Percent Difference in Nocturnality", main = "Percent Difference in Nocturnality") +
  scale_color_manual(values = my_colors) +  # Set custom colors
  guides(fill = guide_legend(title = NULL), color = guide_legend(title = NULL)) + # Remove legend title
  scale_fill_manual(values = my_colors) +
  geom_text(aes(x = Species, y = -.34, label = Species), hjust = 0, vjust = 0.5, color = "black")


