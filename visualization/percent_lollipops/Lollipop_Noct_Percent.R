# lollipop chart for noct percentage
# Margaret Mercer
# June 25, 2024

library(ggplot2)
library(extrafont)
library(tidyverse)
library(scales)
library(grid)
library(png)

data <- read_csv("results/nocturnality.csv")

data <- data[c(1:11), ]

# add common names
data$Common_Name <- c("Puma",
                       "Gray Wolf",
                       "White-tailed Deer",
                       "Mule Deer",
                       "Elk",
                       "Moose",
                       "Coyote",
                       "Bobcat",
                       "Raccoon",
                       "Red Fox",
                       "Striped Skunk")

data$Trend[2] <- "slightly decreasing"

data$Species <- factor(data$Species, levels = rev(unique(data$Species)))

# percent change column!
data <- data %>% mutate(Percent_Change = Noct_Diff/Noct_Low)

# Define custom colors
my_colors <- c("Increase" = "#0B5401", "Slight Increase" = "#77A87C", "No Change" = "steelblue", "Slight Decrease" = "#C67976", "Decrease" = "#8B0000", "White" = "white", "Shaded" = "#E5E5E5")

# Create the lollipop chart with legend title removed and custom colors
lol <- ggplot(data, aes(x = Species, y = Percent_Change,
                 fill = ifelse(Trend == "increasing", "Increase",
                               ifelse(Trend == "slightly increasing", "Shaded",
                                      ifelse(Trend == "slightly decreasing", "White",
                                             ifelse(Trend == "decreasing", "Decrease", "No Change")))),
                 color = ifelse(Trend == "increasing", "Increase",
                                ifelse(Trend == "slightly increasing", "Increase",
                                       ifelse(Trend == "slightly decreasing", "Decrease",
                                              ifelse(Trend == "decreasing", "Decrease", "No Change"))))
)) +
  geom_segment(aes(xend = Species, yend = 0)) +
  geom_rect(data = data[data$Type == "herbivore", ],
            aes(xmin = as.numeric(Species) - 0.5, xmax = as.numeric(Species) + 0.5,
                ymin = -Inf, ymax = Inf),
            fill = "#E5E5E5", color = NA) +
  geom_segment(aes(xend = Species, yend = 0)) +
  geom_point(shape = 21, size = 3) +
  scale_y_continuous(expand = c(0, 0), 
                     limits = c((min((data$Percent_Change)) - 0.15), 
                                (max((data$Percent_Change)) + 0.1)), 
                     labels = percent_format()) +
  coord_flip() +
  theme_classic () +
  theme(axis.title.y = element_blank(),
        panel.border = element_blank(),
        legend.position = "none",
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        text = element_text(family = "Helvetica", size = 15)) +
  geom_hline(yintercept = 0, color = "darkgray") +
  labs(x = NULL, 
       y = "Percent Difference in Nocturnality", 
       main = "Percent Difference in Nocturnality") +
  scale_color_manual(values = my_colors) +  # Set custom colors
  guides(fill = guide_legend(title = NULL), color = guide_legend(title = NULL)) + # Remove legend title
  scale_fill_manual(values = my_colors) +
  geom_text(aes(x = Species, 
                y = (min((Percent_Change)) - 0.14), 
                label = Common_Name), 
            hjust = 0, 
            vjust = 0.5, 
            color = "black")
lol

sun <- readPNG("visualization/pngs/sun.png") %>% rasterGrob(interpolate=TRUE)
moon <- readPNG("visualization/pngs/moon.png") %>% rasterGrob(interpolate=TRUE)

lol +
  annotation_custom(moon, 
                    xmin=0.5, 
                    xmax=5.5, 
                    ymin=0, 
                    ymax=0.5) +
  annotation_custom(sun, 
                    xmin=1, 
                    xmax=5.5, 
                    ymin=-0.2, 
                    ymax=0)
