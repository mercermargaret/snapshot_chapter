# conceptual framework for four hypotheses of human avoidance/attraction in predator-prey interactions
# Margaret Mercer
# June 8, 2024

# load packages
library(ggplot2)
library(grid)
library(gridExtra)
library(png)
library(tidyverse)

# import silhouettes
human_image <- readPNG("visualization/human_black.png")
prey_image <- readPNG("visualization/muledeer_black.png")
pred_image <- readPNG("visualization/puma_black.png")
hum <- rasterGrob(human_image, interpolate=TRUE)
pry <- rasterGrob(prey_image, interpolate=TRUE)
prd <- rasterGrob(pred_image, interpolate=TRUE)

# Define the function for an upside down parabola
parabola <- function(x, a = -1, b = 0, c = 0) {
  return(a * (x - b)^2 + c)
}

# Create a sequence of x values
x_value_pred <- seq(-45, 15, length.out = 100)
x_value_prey <- seq(-30, 30, length.out = 100)
x_value_human <- seq(-15, 45, length.out = 100)

# Create a data frame for the three parabolas
df <- data.frame(
  x = rep(c(x_value_pred, x_value_prey, x_value_human)),
  y = c(parabola(x_value_pred, b = -15), parabola(x_value_prey, b = 0), parabola(x_value_human, b = 15)),
  parabola = factor(rep(c("Parabola 1", "Parabola 2", "Parabola 3"), each = 100))
)

df <- df %>%
  mutate(color = case_when(
    parabola == "Parabola 1" ~ "#CB429F",
    parabola == "Parabola 2" ~ "#0075C4",
    parabola == "Parabola 3" ~ NA))

# Create the plot using ggplot
mat <- ggplot(df, aes(x = x, y = y, color = parabola)) +
  geom_polygon(size = 1, fill = df$color, alpha = 0.3) +
  labs(title = "Mutual Attraction Hypothesis",
       x = "x", y = "y") +
  scale_color_manual(values = c("#CB429F", "#0075C4", "darkgray")) +
  xlim(-80, 50) +
  ylim(-900, 200) +
  theme(legend.title = element_blank()) +
  theme_classic() +
  theme(axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        panel.border = element_blank(),
        legend.position = "none",
        # axis.text.y = element_blank(),
        # axis.text.x = element_blank(),
        axis.ticks = element_blank(),
        axis.line = element_blank(),
        plot.title = element_text(size = 10, hjust = 0.5))
mat

mat_s <- mat + 
  annotation_custom(hum, xmin=-50, xmax=110, ymin=-50, ymax=250) +
  annotation_custom(pry, xmin=-97, xmax=103, ymin=20, ymax=270) +
  annotation_custom(prd, xmin=-115, xmax=85, ymin=-0, ymax=200)

# change gm_line to gm_polygon, and fill = (change for different colors, NA for human)
# this works great
# 
# # now we try to shade the area under the first two parabolas (pred and prey)
# # get overlap for area under the two curves
# intersection_points <- df %>%
#   filter(parabola == "Parabola 1") %>%
#   inner_join(df %>% filter(parabola == "Parabola 2"), by = "x", suffix = c(".1", ".2")) %>%
#   filter(y.1 <= y.2)
# # so it seems like this didn't work at all
# 
# # Create polygon data for the shaded area
# polygon_data <- bind_rows(
#   intersection_points %>%
#     select(x, y = y.1),
#   intersection_points %>%
#     arrange(desc(x)) %>%
#     select(x, y = y.2))
# 
# 
# ggplot() +
#   geom_line(data = df, aes(x = x, y = y, color = parabola, fill = parabola), size = 1) +
#   labs(title = "Three Upside Down Parabolas", x = NULL, y = NULL) +
#   scale_color_manual(values = c("red", "blue", "gray")) +
#   xlim(-80, 50) +
#   theme_classic() +
#   theme(
#     legend.title = element_blank(),
#     legend.position = "none",
#     axis.text = element_blank(),
#     axis.ticks = element_blank(),
#     axis.line = element_blank(),
#     panel.border = element_blank()
#   )
# 
# ggplot() +
#   geom_polygon(data = polygon_data, aes(x = x, y = y), fill = "grey", alpha = 0.5)
# # this just comes out as a gray square :')




# lets put a pin in that and move on to making the curves look the way we want for the different quadrants

# human shield
# Create a sequence of x values
x_value_pred <- seq(-75, -15, length.out = 100)
x_value_prey <- seq(-30, 30, length.out = 100)
x_value_human <- seq(-15, 45, length.out = 100)

# Create a data frame for the three parabolas
df <- data.frame(
  x = rep(c(x_value_pred, x_value_prey, x_value_human)),
  y = c(parabola(x_value_pred, b = -45), parabola(x_value_prey, b = 0), parabola(x_value_human, b = 15)),
  parabola = factor(rep(c("Parabola 1", "Parabola 2", "Parabola 3"), each = 100))
)

# Create the plot using ggplot
hs <- ggplot(df, aes(x = x, y = y, color = parabola)) +
  geom_line(size = 1) +
  labs(title = "Human Shield Hypothesis",
       x = "x", y = "y") +
  scale_color_manual(values = c("#CB429F", "#0075C4", "darkgray")) +
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
hs

hs_s <- hs + 
  annotation_custom(hum, xmin=-50, xmax=110, ymin=-50, ymax=250) +
  annotation_custom(pry, xmin=-97, xmax=103, ymin=20, ymax=270) +
  annotation_custom(prd, xmin=-145, xmax=55, ymin=-0, ymax=200)



# mutual avoidance
# Create a sequence of x values
x_value_pred <- seq(-75, -15, length.out = 100)
x_value_prey <- seq(-60, 0, length.out = 100)
x_value_human <- seq(-15, 45, length.out = 100)


# Create a data frame for the three parabolas
df <- data.frame(
  x = rep(c(x_value_pred, x_value_prey, x_value_human)),
  y = c(parabola(x_value_pred, b = -45), parabola(x_value_prey, b = -30), parabola(x_value_human, b = 15)),
  parabola = factor(rep(c("Parabola 1", "Parabola 2", "Parabola 3"), each = 100))
)

# Create the plot using ggplot
mav <- ggplot(df, aes(x = x, y = y, color = parabola)) +
  geom_line(size = 1) +
  labs(title = "Mutual Avoidance Hypothesis",
       x = "x", y = "y") +
  scale_color_manual(values = c("#CB429F", "#0075C4", "darkgray")) +
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
mav
mav_s <- mav + 
  annotation_custom(hum, xmin=-50, xmax=110, ymin=-50, ymax=250) +
  annotation_custom(pry, xmin=-127, xmax=73, ymin=20, ymax=270) +
  annotation_custom(prd, xmin=-145, xmax=55, ymin=-0, ymax=200)


# predator attraction
# Create a sequence of x values
x_value_pred <- seq(-30, 30, length.out = 100)
x_value_prey <- seq(-75, -15, length.out = 100)
x_value_human <- seq(-15, 45, length.out = 100)


# Create a data frame for the three parabolas
df <- data.frame(
  x = rep(c(x_value_pred, x_value_prey, x_value_human)),
  y = c(parabola(x_value_pred, b = 0), parabola(x_value_prey, b = -45), parabola(x_value_human, b = 15)),
  parabola = factor(rep(c("Parabola 1", "Parabola 2", "Parabola 3"), each = 100))
)

# Create the plot using ggplot
pa <- ggplot(df, aes(x = x, y = y, color = parabola)) +
  geom_line(size = 1) +
  labs(title = "Predator Attraction Hypothesis",
       x = "x", y = "y") +
  scale_color_manual(values = c("#CB429F", "#0075C4", "darkgray")) +
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
pa

pa_s <- pa + 
  annotation_custom(hum, xmin=-50, xmax=110, ymin=-50, ymax=250) +
  annotation_custom(pry, xmin=-142, xmax=58, ymin=20, ymax=270) +
  annotation_custom(prd, xmin=-100, xmax=100, ymin=-0, ymax=200)


# stick the four curves together
g <- grid.arrange(pa_s, mat_s, mav_s, hs_s, ncol=2) + 
  annotation_custom(hum, xmin=-500, xmax=500, ymin=-500, ymax=500)



arrow_data <- data.frame(
  x = c(0, 0),
  y = c(-200, 200)  # y-values are kept constant to draw a horizontal arrow
)

# Add the double-ended arrow using geom_segment
g + 
  geom_line()

arrows(-100, 50, 100, 50, length = 100, angle = 30,
       code = 2, col = "red", lty = "solid",
       lwd = 1)



