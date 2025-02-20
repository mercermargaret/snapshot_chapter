# messing around with multispecies occupancy model from Rota 2016
# margaret mercer
# august 2, 2024


# setup ####
# load packages
library(rprojroot)
library(tidyverse)
library(rstan)
library(xlsx) # it didn't like this at first but I fixed it by downloading Java

# clear workspace
rm(list=ls())

# set wd temporarily to folder with all the example data
setwd("/Users/mmercer3/Downloads/multispecies_occupancy_example")

# wranglin' ####
# this whole section runs
# importing detection / non-detection data
bobcat <- as.matrix(read.csv('Bobcat.csv', F))
coyote <- as.matrix(read.csv('Coyote.csv', F))
gryfox <- as.matrix(read.csv('Gray Fox.csv', F))
redfox <- as.matrix(read.csv('Red Fox.csv', F))

# total number of camera days
cday <- apply(bobcat, 1, function(x) sum(is.na(x) == F))

# importing covariate (site level) data
psi.cov <- read.csv('psi covariates.csv')
p.cov <- read.csv('p covariates.csv')

# CREATING DESIGN MATRIX
X.array <- array(0, dim = c(nrow(bobcat), 16, 32)) # why this?

# making vectors out of covariates
d5km <- scale(psi.cov$Dist_5km)[, 1]
hden <- scale(psi.cov$HDens_5km)[, 1]
lati <- scale(psi.cov$Latitude)[, 1]
long <- scale(psi.cov$Longitude)[, 1]
lbyl <- lati * long
hike <- scale(psi.cov$People_site * 1000 / cday)

# importing the general form of the design matrix
dm <- read.xlsx('Design Matrix.xlsx', 3, rowIndex = 3:18, colIndex = 2:33,
                header = F) # i dont really undersetand the design matrix thing

# filling in the design matrix
for(i in 1:nrow(bobcat)){
  for(j in 1:15){
    X.array[i, j, dm[j, ] == 1] <-
      c(1, lati[i], long[i], lbyl[i], hike[i],
        1, lati[i], long[i], lbyl[i], hden[i],
        1, lati[i], long[i], lbyl[i], hden[i],
        1, lati[i], long[i], lbyl[i], hike[i],
        1, d5km[i],
        1, hden[i],
        1, hden[i],
        1, d5km[i],
        1, hden[i],
        1, d5km[i])[dm[j, ] == 1]
  }
}

# OBSERVED Y
y1max <- apply(bobcat, 1, max, na.rm = T)
y2max <- apply(coyote, 1, max, na.rm = T)
y3max <- apply(gryfox, 1, max, na.rm = T)
y4max <- apply(redfox, 1, max, na.rm = T)

# Vectorizing detection / non-detection
Y1 <- Y2 <- Y3 <- Y4 <- trail <- dd <- numeric()
for(i in 1:nrow(bobcat)){
  Y1 <- c(Y1, bobcat[i, 1:cday[i]])
  Y2 <- c(Y2, coyote[i, 1:cday[i]])
  Y3 <- c(Y3, gryfox[i, 1:cday[i]])
  Y4 <- c(Y4, redfox[i, 1:cday[i]])
  trail <- c(trail, rep(psi.cov$Trail[i], cday[i]))
  dd <- c(dd, rep(p.cov$Det_dist[i], cday[i]))
}

# Stan does not support ragged indexing.  This is a work-around
start <- 1
for(i in 2:nrow(bobcat)){
  start <- c(start, sum(cday[1:(i - 1)]) + 1)
}

data <- list(
  K = ncol(X.array[1, , ]),
  L = 3,
  N = nrow(bobcat),
  NJ = length(Y1),
  S = 16,
  obs = cday,
  start = start,
  x = X.array,
  trl = trail,
  dd = scale(dd)[, 1],
  I1 = y1max, I2 = y2max, I3 = y3max, I4 = y4max,
  Y1 = Y1, Y2 = Y2, Y3 = Y3, Y4 = Y4
)

inits <- function(){
  list(
    a1 = rnorm(3),
    a2 = rnorm(3),
    a3 = rnorm(3),
    a4 = rnorm(3),
    beta = rnorm(ncol(X.array[1, , ])))
}

params <- c('a1', 'a2', 'a3', 'a4', 'beta', 'll')



# model ####
# source('Formatting Data.R') # when I run this it errors

# # takes about 12 hours to run
# fit <- stan('model3.stan', data = data, pars = params, chains = 2, init = inits,
#             iter = 2000, warmup = 1000, thin = 1)

# checking convergence
max(summary(fit)[[1]][, 'Rhat'])

post.lik <- extract(fit, 'll')

# WAIC
-2 * sum(log(apply(exp(post.lik[[1]]), 2, mean))) +
  2 * sum(apply(post.lik[[1]], 2, var))

# summarizing slope coefficients
post.beta <- extract(fit, 'beta')

rbind(apply(post.beta[[1]], 2, quantile, probs = 0.025),
      apply(post.beta[[1]], 2, mean),
      apply(post.beta[[1]], 2, quantile, probs = 0.975))







# reset wd to my project!
setwd("/Users/mmercer3/Documents/R_files/snapshot_chapter")
