# visualize occupancy
# margaret mercer
# august 16, 2024

# install packages
library(tidyverse)
library(ggplot2)

# import data
results <- read.csv("results/occupancy_modeling_results.csv")

# reformat data slightly...

# ... adding column for pred/prey pairing ...
results$Pair <- paste(results$Predator, results$Prey, sep = "/")
results$Pair <- factor(results$Pair, levels = rev(unique(results$Pair)))

# ... and a column for significance (sorry, Bob)
results$Significant <- ifelse(results$`p.value` < 0.1, "yes", "no")

# plot
ggplot(results, aes(x = Effect.of.Predator.Presence.on.Prey, y = Pair)) +
  geom_line() +
  geom_point(aes(color = Human.Disturbance), 
             alpha = ifelse(results$Significant == "no", 0.2, 1),
             size = 3) +
  theme(legend.position = "bottom")
