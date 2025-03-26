# a bar graph to show the difference in nocturnality of each species
# margaret mercer
# march 22 2025

library(ggplot2)
library(scales)
library(tidyverse)

# import data
data <- read.csv("results/nocturnality.csv")

# get rid of humans
data <- data[1:11,]

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

data$Common_Name <- factor(data$Common_Name, levels = rev(unique(data$Common_Name)))

# Reshape the data for ggplot
data_long <- data %>%
  gather(key = "Noct_Type", value = "value", Noct_Low, Noct_High)

# assign colors. red if Noct_Type is Noct_High AND Noct_Diff is < 0. green if Noct_Type is Noct_High AND Noct_Diff is > 0. Black if neither.
data_long <- data_long %>%
  mutate(Color = case_when(
    Noct_Type == "Noct_High" & Noct_Diff < 0 ~ "High Human Activity (Decrease)",  # Noct_High and Noct_Diff < 0
    Noct_Type == "Noct_High" & Noct_Diff > 0 ~ "High Human Activity (Increase)",  # Noct_High and Noct_Diff > 0
    TRUE ~ "Low Human Activity"  # All other cases
  ))

# plot
ggplot(data_long, aes(x = Common_Name, y = value, fill = Color)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7, color = "white") +  # Bar chart
  geom_rect(data = data_long[data_long$Type == "herbivore", ],
            aes(xmin = X + 2.5, xmax = X + 3.5, # wasn't showing up in the right spot so I manually adjusted this
                ymin = -Inf, ymax = Inf),
            fill = "#E5E5E5", color = NA) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7) +  # Bar chart
  coord_flip() +  # Flip the axes for horizontal bars
  scale_y_continuous(labels = percent_format()) +
  scale_fill_manual(values = c("High Human Activity (Decrease)" = "#AE0000", 
                               "High Human Activity (Increase)" = "#057A00", 
                               "Low Human Activity" = "#949494")) +  # Custom colors
  labs(x = NULL, y = "Rate of Nighttime Activity", fill = "Nocturnal Type") +
  theme_minimal() +
  theme(
    # panel.grid.major.x = element_line(color = "grey", size = 0.5),  # Keep x grid lines
    axis.title.x = element_text(size = 40),
    axis.title.y = element_text(size = 40),
    axis.text.x = element_text(size = 30),
    axis.text.y = element_text(size = 30),
    panel.grid.major = element_blank(), # Remove major grid lines
    panel.grid.minor = element_blank(), # Remove minor grid lines
    axis.line = element_line(color = "black", linewidth = 0.5), # Keep x-axis line
    panel.border = element_blank(),     # Remove panel borders
    axis.ticks = element_blank()        # Remove axis ticks
  )



