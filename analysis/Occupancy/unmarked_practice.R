# practice for unmarked package
# margaret mercer
# july 5, 2024

library(unmarked)
wt <- read.csv(system.file("csv","widewt.csv", package="unmarked"))

# Presence/absence matrix
y <- wt[,2:4]

# Site and observation covariates
siteCovs <-  wt[,c("elev", "forest", "length")]
obsCovs <- list(date=wt[,c("date.1", "date.2", "date.3")]) 

umf <- unmarkedFrameOccu(y = y, siteCovs = siteCovs, obsCovs = obsCovs)
summary(umf)

# Fit a null occupancy model and a model with covariates, using the occu function:
(mod_null <- occu(~1~1, data=umf))

(mod_covs <- occu(~date~elev, data=umf))

# rank them using AIC
fl <- fitList(null=mod_null, covs=mod_covs)
modSel(fl)

# Estimate occupancy probability using the top-ranked model at the first six sites:
head(predict(mod_covs, type='state'))

# Predict occupancy probability at a new site with given covariate values:
nd <- data.frame(elev = 1.2)
predict(mod_covs, type="state", newdata=nd)
