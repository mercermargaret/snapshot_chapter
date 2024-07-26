# experimenting with adding arrows to ggplot and then to grob
# margaret mercer
# june 27, 2024

library(plotly)
library(ggplot2)

# Data frame with two continuous variables and two factors
set.seed(0)
x <- rep(1:10, 4)
y <- c(rep(1:10, 2)+rnorm(20)/5, rep(6:15, 2) + rnorm(20)/5)
treatment <- gl(2, 20, 40, labels=letters[1:2])
replicate <- gl(2, 10, 40)
d <- data.frame(x=x, y=y, treatment=treatment, replicate=replicate)

p <- ggplot(d, aes(x=x, y=y, colour=treatment, group=interaction(treatment, replicate))) +
  geom_point() + geom_line()

fig <- ggplotly(p)

fig



pa + geom_hline(yintercept=0)

