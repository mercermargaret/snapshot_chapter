# change in nocturnality as a function of beginning nocturnality
# margaret mercer
# july 1 2024

# packages
library(ggplot2)
library(scales)
library(tidyverse)

data <- read_csv("results/nocturnality.csv")

# cut out humans
data <- data[1:11,]

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

data$Species <- factor(data$Species, levels = rev(unique(data$Species)))

# percent change column!
data <- data %>% mutate(Percent_Change = Noct_Diff/Noct_Low)

# plot the schtuff
# 
# plot(data$Percent_Change ~ data$Noct_Overall)
# 
# ggplot(data, aes(Noct_Low, Percent_Change)) +
#   geom_point(size = 3, aes(color = Type)) +
#   theme_classic() + 
#   geom_smooth(method='lm', formula=y~x, se = FALSE, color = "black")

# divide percent change by the total amount it could have changed (1-noct)
data <- data %>% mutate(Potential_Change = (1-Noct_Low))
data <- data %>% mutate(Prop_Change_to_Potential_Change = Noct_Diff/Potential_Change)

# ggplot(data, aes(Prop_Change_to_Potential_Change, Percent_Change, fill = Type)) +
#   geom_smooth(method = "lm", se = TRUE, level = 0.95, col = "black", fill = "lightgrey", lwd = 3) +
#   geom_point(size = 3, aes(color = Type)) +
#   scale_y_continuous(labels = percent_format()) +
#   scale_x_continuous(labels = percent_format()) +
#   labs(x = "Potential Percent Change in Nocturnality", 
#        y = "Percent Change in Nocturnality") +
#   scale_color_manual(values = c("carnivore" = "red",
#                                 "herbivore" = "blue",
#                                 "mesocarnivore" = "green")) +
#   theme_classic() +
#   theme(
#     plot.title = element_text(size = 40, face = "bold", hjust = -0.05), 
#     axis.title.x = element_text(size = 30, face = "bold"),      
#     axis.title.y = element_text(size = 30, face = "bold"),
#     axis.text = element_text(size = 30, face = "bold") 
#   )

# lets see what happens if we plot change in noct as a function of starting noct
ggplot(data, aes(Noct_Overall, Percent_Change, fill = Type)) + 
  geom_smooth(method = "lm", se = TRUE, level = 0.95, col = "black", fill = "lightgrey", lwd = 3) + 
  geom_point(size = 8, aes(color = Type)) + 
  geom_text(aes(label = Common_Name), nudge_y = 0.02, nudge_x = 0, size = 5) + # Add point labels
  scale_y_continuous(labels = scales::percent_format()) + 
  scale_x_continuous(labels = scales::percent_format()) + 
  labs(x = "Overall Nocturnality", 
       y = "Percent Change in Nocturnality") + 
  scale_color_manual(values = c("carnivore" = "red", 
                                "herbivore" = "blue", 
                                "mesocarnivore" = "green")) + 
  theme_classic() + 
  theme(
    plot.title = element_text(size = 40, face = "bold", hjust = -0.05), 
    axis.title.x = element_text(size = 30, face = "bold"),      
    axis.title.y = element_text(size = 30, face = "bold"), 
    axis.text = element_text(size = 30, face = "bold") 
  )

