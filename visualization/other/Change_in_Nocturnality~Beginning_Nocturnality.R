# change in nocturnality as a function of beginning nocturnality
# margaret mercer
# july 1 2024

data <- read_csv("data/Nocturnality_Results - USE THIS ONE.csv")

data <- data[c(1:2, 5:9, 14, 16:18), ]

data$Species <- factor(data$Species, levels = rev(unique(data$Species)))

# percent change column!
data <- data %>% mutate(Percent_Change = Noct_Diff/Noct_Low)

# plot the schtuff

plot(data$Percent_Change ~ data$Noct_Overall)

ggplot(data, aes(Noct_Low, Percent_Change)) +
  geom_point(size = 3, aes(color = Type)) +
  theme_classic() + 
  geom_smooth(method='lm', formula=y~x, se = FALSE, color = "black")

# divide percent change by the total amount it could have changed (1-noct)
data <- data %>% mutate(Potential_Change = (1-Noct_Low))
data <- data %>% mutate(Prop_Change_to_Potential_Change = Noct_Diff/Potential_Change)

ggplot(data, aes(Prop_Change_to_Potential_Change, Percent_Change)) +
  geom_point(size = 3, aes(color = Type)) +
  theme_classic() + 
  geom_smooth(method='lm', formula=y~x, se = FALSE, color = "black")
