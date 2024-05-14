# lollipop chart
# Margaret Mercer
# May 8, 2024

library(ggplot2)
library(extrafont)
library(tidyverse)
library(scales)

data <- read_csv("data/Overlap_Results.csv")

data <- data[1:16,]

data$Pairing <- paste(data$Predator, data$Prey, sep = "/")

# Add a new variable to specify shading groups
data <- data %>%
  mutate(Group = rep(1:4, each = 4))

# my_colors <- c("increasing" = "darkgreen", equal = "steelblue", "decreasing" = "darkred")

# my_colors <- c("positive" = "darkgreen", "negative" = "darkred")

# ggplot(data, aes(x = Pairing, y = Difference, color = ifelse(Trend >= 0, "positive", "negative"))) +
#   geom_segment(aes(xend = Pairing, yend = 0)) +
#   geom_point(shape = 21, size = 3) +
#   scale_y_continuous(expand = c(0, 0), limits = c(-0.5, 0.5)) +
#   coord_flip() +  # Rotate the plot
#   theme_minimal() +
#   theme(axis.title.y = element_blank()) +  # Remove y-axis label
#   labs(x = NULL, y = "Value") +  # Remove x-axis label
#   scale_x_discrete(position = "top") +  # Put categories on top
#   scale_fill_manual()  # Set point fill color

# Define custom colors
my_colors <- c("Increase" = "darkgreen", "No Change" = "steelblue", "Decrease" = "darkred")

# Create the lollipop chart with legend title removed and custom colors
ggplot(data, aes(x = Pairing, y = Difference,
                 fill = ifelse(Difference > 0, "Increase", ifelse(Difference < 0, "Decrease", "No Change")),
                 color = ifelse(Difference > 0, "Increase", ifelse(Difference < 0, "Decrease", "No Change"))
)) +
  geom_segment(aes(xend = Pairing, yend = 0)) +
  geom_point(shape = 21, size = 3) +
  # geom_rect(data=df, aes(NULL,NULL,xmin=start,xmax=end,fill=party),
              # ymin=0,ymax=16000, colour="white", size=0.5, alpha=0.2) +
  # scale_fill_manual(values = c("Puma" = "red", "Gray Wolf" = "blue")) +
  scale_y_continuous(expand = c(0, 0), limits = c(-.50, .50), labels = percent_format()) +
  coord_flip() +
  theme_minimal() +
  theme(axis.title.y = element_blank(),
        panel.border = element_blank(),
        legend.position = "none",
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        text = element_text(family = "Helvetica", size = 15)) +
  geom_hline(yintercept = 0, color = "darkgray") +
  labs(x = NULL, y = "Change in Temporal Overlap As Human Presence Increases") +
  scale_color_manual(values = my_colors) +  # Set custom colors
  guides(fill = guide_legend(title = NULL), color = guide_legend(title = NULL)) + # Remove legend title
  scale_fill_manual(values = my_colors)



