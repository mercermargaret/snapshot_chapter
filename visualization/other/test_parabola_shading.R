
# load packages
library(ggplot2)
library(gridExtra)

# Define the function for an upside down parabola
parabola <- function(x, a = -1, b = 0, c = 0) {
  return(a * (x - b)^2 + c)
}

# Create a sequence of x values
x_value_pred <- seq(-45, 15, length.out = 100)
x_value_prey <- seq(-30, 30, length.out = 100)
x_value_human <- seq(-15, 45, length.out = 100)

b_pred = -15
b_prey = 0

# Create a data frame for the three parabolas
df <- data.frame(
  x = rep(c(x_value_pred, x_value_prey, x_value_human)),
  y = c(parabola(x_value_pred, b = -15), parabola(x_value_prey, b = 0), parabola(x_value_human, b = 15)),
  parabola = factor(rep(c("Parabola 1", "Parabola 2", "Parabola 3"), each = 100))
)

df_new <- df %>% 
  filter(x >= b_pred & parabola == "Parabola 1" | x <= b_prey & parabola == "Parabola 2") %>% 
  mutate(new_y = case_when(parabola == "Parabola 1" ~ parabola(.$x, b = (b_prey)),
                           parabola == "Parabola 2" ~ parabola(.$x, b = b_pred))) %>% 
  pivot_wider(names_from = parabola, values_from = x) %>% 
  mutate(ymin = -900,
         ymax = case_when(`Parabola 1` >= `Parabola 2` ~ pmin(y, new_y)))

df_new <- df_new %>% 
  drop_na()

# Create the plot using ggplot
mat <- ggplot() +
  geom_line(data = df, aes(x = x, y = y, color = parabola), size = 1) +
  labs(title = "Mutual Attraction Hypothesis",
       x = "x", y = "y") +
  scale_color_manual(values = c("#CB429F", "#0075C4", "darkgray")) +
  geom_ribbon(data = df_new, aes(x = `Parabola 1`, ymin = ymin, ymax = ymax), fill = "lightgray") +
  geom_ribbon(data = df_new, aes(x = `Parabola 2`, ymin = ymin, ymax = ymax), fill = "lightgray") +
  xlim(-80, 50) +
  ylim(-900, 200) +
  theme(legend.title = element_blank()) +
  theme_classic() +
  theme(axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        panel.border = element_blank(),
        legend.position = "none",
        axis.text.y = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks = element_blank(),
        axis.line = element_blank(),
        plot.title = element_text(size = 10, hjust = 0.5))
mat

