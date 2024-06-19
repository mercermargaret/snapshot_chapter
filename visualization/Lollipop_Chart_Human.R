# lollipop chart for overlap of each species and human
# Margaret Mercer
# June 19, 2024

library(ggplot2)
library(extrafont)
library(tidyverse)
library(scales)

data <- read_csv("data/Human_Overlap_Results - Human Overlap.csv")

data$Species <- factor(data$Species, levels = rev(unique(data$Species)))

data <- data[c(1, 2, 4:8, 14:18), ] # subset to only the species we want

# Define custom colors
my_colors <- c("Increase" = "darkgreen", "No Change" = "steelblue", "Decrease" = "darkred")

# Create the lollipop chart with legend title removed and custom colors
ggplot(data, aes(x = Species, y = Difference,
                 fill = ifelse(Difference > 0, "Increase", ifelse(Difference < 0, "Decrease", "No Change")),
                 color = ifelse(Difference > 0, "Increase", ifelse(Difference < 0, "Decrease", "No Change"))
)) + 
  # geom_rect(data = data[data$Type == "herbivore", ],
  #           aes(xmin = as.numeric(Species) - 0.5, xmax = as.numeric(Species) + 0.5,
  #               ymin = -Inf, ymax = Inf),
  #           fill = "gray", alpha = 0.5, color = NA) + 
  geom_segment(aes(xend = Species, yend = 0)) +
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
  labs(x = NULL, y = "Difference in Temporal Overlap", main = "Difference in Temporal Overlap between Animals and Humans in Low versus High Human activity") +
  scale_color_manual(values = my_colors) +  # Set custom colors
  guides(fill = guide_legend(title = NULL), color = guide_legend(title = NULL)) + # Remove legend title
  scale_fill_manual(values = my_colors) +
  geom_text(aes(x = Species, y = -.29, label = Species), hjust = 0, vjust = 0.5, color = "black") # Adjust position to be just inside the plot area

