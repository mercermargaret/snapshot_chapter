# dumbell to show how nocturnality changes
# margaret mercer
# march 25, 2025

# install packages
library(tidyverse)
library(ggplot2)
library(ggalt)
library(scales)
library(glue)

# import data
data <- read_csv("results/nocturnality.csv")

data <- data[c(1:11), ] # get rid of humans, we dont care

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

data$Common_Name <- factor(data$Common_Name, levels = rev(unique(data$Common_Name))) # make it a factor so plot is in correct order

# ... and a column for significance (sorry, Bob)
data$Significant <- ifelse(data$`p.value` < 0.1, "yes", "no")

# plot
ggplot(data, aes(x = Noct_Low, xend = Noct_High, y = Common_Name)) +
  geom_dumbbell(color = ifelse(data$Noct_Diff < 0, "red", "green"),
                colour_x = "grey",
                colour_xend = "black",
                size = 3) +
  theme_minimal() +
  labs(title = "Dumbbell Plot Example", 
       x = "Frequency of Night Activity", y = "Species")

ggplot(data, aes(x = Noct_Low, xend = Noct_High, y = Common_Name)) +   
  # geom_dumbbell(
  #   color = ifelse(data$Noct_Diff < 0, "red", "green"),
  #   # colour_x = ifelse(data$Noct_Diff < 0, "red", "green"),  # Lighter color
  #   # colour_xend = ifelse(data$Noct_Diff < 0, "red", "green"),  # Color based on Noct_Diff
  #   size = 3
  # ) +
  geom_segment(aes(x = Noct_Low, y = Common_Name, xend = Noct_High, yend = Common_Name), color = "white") +  # this plots blank segments, necessary to preserve correct species order
  geom_rect(data = data[data$Type == "herbivore", ],
            aes(ymin = as.numeric(Common_Name) - 0.5, ymax = as.numeric(Common_Name) + 0.5,
                xmin = -Inf, xmax = Inf),
            fill = "#E5E5E5", color = NA) +
  geom_segment(
    aes(x = Noct_Low - 0.004, y = Common_Name, xend = Noct_High, yend = Common_Name),
    color = ifelse(data$Noct_Diff < 0, "#AE0000", "#057A00"),
    arrow = arrow(length = unit(0.5, "cm")),
    lwd = 2
  ) +
  scale_x_continuous(labels = percent_format(), limits = c(min(data$Noct_High), max(data$Noct_High) + .01)) +
  geom_point(aes(Noct_Low),
             color = ifelse(data$Noct_Diff < 0, "#AE0000", "#057A00"),
             pch = ifelse(data$Significant == "no", 1, 16),
             size = 5) +
  # geom_point(aes(Noct_High),
  #            color = ifelse(data$Noct_Diff < 0, "#AE0000", "#057A00"),
  #            size = 6) +
  theme_minimal() +   
  labs(
    x = "Frequency of Nocturnal Activity", 
    y = NULL
  ) +
  theme(
    # panel.grid.major.x = element_line(color = "grey", size = 0.5),  # Keep x grid lines
    axis.title.x = element_text(size = 30),
    axis.title.y = element_text(size = 30),
    axis.text.x = element_text(size = 25),
    axis.text.y = element_text(size = 30, color = "black"),
    panel.grid.major = element_blank(), # Remove major grid lines
    panel.grid.minor = element_blank(), # Remove minor grid lines
    axis.line = element_line(color = "black", linewidth = 0.5), # Keep x-axis line
    panel.border = element_blank(),     # Remove panel borders
    axis.ticks = element_blank()        # Remove axis ticks
  )
