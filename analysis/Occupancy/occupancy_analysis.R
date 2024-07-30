# occupancy analysis
# margaret mercer (initial code provided by javan bauder)
# july 24, 2024


# clear workspace
rm(list=ls())

library(tidyverse)
library(unmarked)
library(AICcmodavg)
library(ggplot2)
library(TMB)


# Single-Season Occupancy Models -----------------
# Read in a csv file of our data --------

# Our encounter histories are represented by columns visit1, visit2, visit3. One nice
# feature of unmarked is that we don't have to concatenate our encounter histories 
# like we do in RMark! We then have two site-level covariates, Native and PatchSize.
# We then have three survey-level covariates as denote by their 1, 2, 3 postscripts.
# Remember for survey-level covariates we need one covariate (i.e., column) per survey/visit.
# Also note that Obs1, Obs2, and Obs3 are 4-level categorical factors. 

hist <- read.csv("data/encounter_histories.csv")
DOY <- read.csv("data/day_of_year.csv")
survey_days <- read.csv("data/survey_days.csv")
site_info <- read.csv("data/site_info.csv")

# turn NAs in human columns to 0
site_info$Humans_Per_Camera <- ifelse(is.na(site_info$Humans_Per_Camera), 0, 
                                      site_info$Humans_Per_Camera)
site_info$Humans_Per_Camera_Per_Day <- ifelse(is.na(site_info$Humans_Per_Camera_Per_Day), 0, 
                                      site_info$Humans_Per_Camera_Per_Day)

# and make sure year is a character not a number!!
site_info$Year <- as.character(site_info$Year)

# Let's z-score standardize our continuous covariates as this is generally a good idea
# when fitting any kind of hierarchical model to help facilitate convergence
DOY_scaled <- scale(as.matrix(DOY[,grep("V",colnames(DOY))]))
days_scaled <- scale(as.matrix(survey_days[,grep("V",colnames(survey_days))]))
# use apply function
# z score standardize all continuous covariates! (you just need to backtransform when you plot it)
# and all your slopes will be directly comparable!

# As a first step, let's pull out the data that will inform each of the following three 
# components of our input data for fitting models in unmarked:
observations <- hist[,grep("V",colnames(hist))]

site_covs <- as.data.frame(site_info[,c("Humans_Per_Camera_Per_Day", 
                                        "Disturbance", 
                                        "Array", 
                                        "Year")]) 
# do we not scale our site level covariates? 
# also should we call each array "array_year" in case the some of the arrays had the same name across years?
site_covs <- site_covs %>%
  rename(
    Humans = "Humans_Per_Camera_Per_Day")

obs_covs <- list(DOY_scaled = DOY_scaled,
                 days_scaled = days_scaled) 
# is this right? that day of year and survey days are observation covariates?
    # is that what helps the model scale number of observations to survey days? 
    # is that how it models for imperfect detection?


# Create an object for model fitting -----------

# Each unmarked model-fitting function will have its own R object that the data will
# need to be placed into in order to fit the model. For example, in order to fit a 
# single-season occupancy model with the occu() function, we format our data as a
# unmarkedFrameOccu object using the unmarkedFrameOccu() function.
# When in doubt about what data input object to use, just use the ? to find the help
# page for your particular model and find the associated input data object.

umf <- unmarkedFrameOccu(y = observations, # Encounter history, must be a data frame or matrix
                         siteCovs = site_covs, # Site covariates, must be a data frame
                         obsCovs = obs_covs) # Observer covariates, must be list of data frames or matrices

# Let's look at our data with the summary function
summary(umf)

# Now fit some single-season occupancy models --------------

# Unlike RMark, all unmarked model fitting functions allow us to specify the formula
# directly into the function without requiring the formulas to be in a list.
# unmarked formulas will generally (always?) use double right-hand formulas. 
# In the case of occu, the order of the formula (from left to right) is
# detection and occupancy.
# Let's start with the dot model.
# a dot model is also called an intercept model. You're basically getting the average occupancy
# and average detection. You're not looking at ANY slope or variation with any factor,
# just the intercepts!

m1 <- occu(~1 ~1, data = umf)
class(m1)
# psi(.)p(.) # there's also this, but I'm not sure what it means

# Let's examine the output

summary(m1)

# Note that our estimates are on the logit scale. Just like RMark, we could use plogis()
# to manually calculate them. But notice what a pain it is to dig the beta
# estimates out of our model object!

plogis(m1@estimates@estimates$state@estimates)

# We can also use unmarked's backTransform() function to do this for us

backTransform(m1, 'state')
backTransform(m1, 'det')

# Now let's model occupancy as a function of humans

m2 <- occu(~ 1 ~ Humans, data = umf)

summary(m2)

# Let's see what occupancy is when Humans is average, which would be 0 because we
# z-score standardized our covariates. Here's where we get a little tricky because 
# we need to combine estimates (intercept and Humans effect). So we need to provide the 
# coefficients= argument both a value for the intercept and Humans, so a vector of two
# numeric values. The linearComb() function will interpret this as 
# 1*Intercept + 0*Humans We can then place linearComb() within backTransform() to
# get our estimates on the real scale.
linearComb(m2, coefficients = c(1, 0), type = 'state')
backTransform(linearComb(m2, coefficients = c(1, 0), type = 'state'))

# We can also see how occupancy varies by Humans by calculating predicted values
# using the predict() function.
# Just like when using predict() with any GLM or GLMM, we need to create a 
# newdata = object, that is, a data frame containing the covariate values at which we
# would like to predict occupancy (or detection)
# Because it is always a good idea to predict within the domain of our data 
# (i.e., not extrapolate beyond the covariate values we actually observed)
# let's create a sequence of wind values spanning the full range of our observed
# wind values.
# Note that if we wanted to predict for detection we would change type = "state" to
# type = "det".

new_data <- data.frame(Humans = seq(min(as.numeric(as.matrix(umf@siteCovs$Humans)),na.rm=T),
                                max(as.numeric(as.matrix(umf@siteCovs$Humans)),na.rm=T),0.1))
new_data <- predict(m2, type = 'state', newdata = new_data, appendData=TRUE)
head(new_data)

plot(Predicted ~ Humans, new_data, ylim = c (0, 1),type = "l", lwd = 3,
     ylab = "Probability of Occupancy", xlab = "Humans per Camera per Day")
lines(new_data$Humans, new_data$lower, lty=3)
lines(new_data$Humans, new_data$upper, lty=3)
# cool!! this works!! but it doesn't make sense that the probability of occupancy would go up 
    # as human presence goes up. Is that what this is saying?

# now another covariate
new_data <- data.frame(Disturbance = seq(min(as.numeric(as.matrix(umf@siteCovs$Disturbance)),na.rm=T),
                                    max(as.numeric(as.matrix(umf@siteCovs$Disturbance)),na.rm=T),0.1))
new_data <- predict(m2, type = 'state', newdata = new_data, appendData=TRUE)
head(new_data)

plot(Predicted ~ Disturbance, new_data, ylim = c (0, 1),type = "l", lwd = 3,
     ylab = "Probability of Occupancy", xlab = "Human Disturbance")
lines(new_data$Disturbance, new_data$lower, lty=3)
lines(new_data$Disturbance, new_data$upper, lty=3)



# testing another covariate (this time for observation)
m3 <- occu(~ 1 + days_scaled ~ 1, data = umf) 
new_data <- data.frame(days_scaled = seq(min(as.numeric(as.matrix(umf@obsCovs$days_scaled)), na.rm = T),
                                    max(as.numeric(as.matrix(umf@obsCovs$days_scaled)), na.rm = T), 0.1))
new_data <- predict(m3, type = 'det', newdata = new_data, appendData = TRUE)
head(new_data)

plot(Predicted ~ days_scaled, new_data, ylim = c (0, 1), type = "l", lwd = 3,
     ylab = "Probability of Detection", xlab = "Number Survey Days")
lines(new_data$days_scaled, new_data$lower, lty = 3)
lines(new_data$days_scaled, new_data$upper, lty = 3)





# Evaluating model goodness-of-fit (GOF) ----------

# Before we proceed with fitting multiple models let's go over how to calculate 
# goodness-of-fit (GOF) for a single-season occupancy model.
# One common GOF test is the MacKenzie and Bailey (2004) GOF test. This relies on
# the calculation of Pearson's chi-square statistic and can be used to estimate
# the overdispersion parameter, c-hat. Essentially what this test does is ask 
# what probability do we have of observing our different encounter histories
# assuming our model is correct. Say we have a lot of encounter histories with only
# one detection but based on our model we would expect to see very few of these 
# single-detection encounter histories. That suggests model lack-of-fit. This 
# also implies that we need some form of simulation or resampling to quantify that probability
# of observing different encounter histories. It also implies that we need
# some form of parametric simulation because we are assuming a given model is true
# (i.e., our model that we are evaluating).

# Also, we need to pick a model with which to evaluate GOF. The standard approach is to
# use your global or most-heavily parameterized model. If your candidate models all contain
# only one or two covariates, you might want to make a global model solely for GOF testing
# but not use it in your candidate set. The idea is that your global model should
# explain the most variation in your data so any model with fewer parameters that has similar
# or better empirical support (e.g., lower AIC) should also have adequate model fit.

# Let's fit a global model just for illustrative purposes

global_mod <- occu(~ DOY_scaled + days_scaled ~ Humans + Development + Habitat, data = umf)
# why no ones here like in the models below?  What do the ones mean?

# The AICcmodavg package we loaded at the beginning
# of the lab contains a function that can run this test for us. 
# This is the mb.gof.test() function. We simply provide the model we wish to evaluate 

system.time(global_MB <- mb.gof.test(global_mod, nsim = 10))
# you say below that 10,000 simulations would take 30 hours. 
# Do you recommend doing that many? Also can you split it up into cores so it doesn't take so long?
# can you use the universitys hpc or whatever to do it better?

# Note that this takes about 24 seconds to run just 10 simulations. So 10,000
# simulations would require about 30 hours! Sounds like a great use for the computer
# over the weekend!
# The error messages are because we have missing observations in our data.

global_MB # how do I know whether it passed the goodness of fit test? what do i do if it didn't?

# Fit and compare additional models ---------------

# Go ahead and fit 4 more models and call them m3, m4, m5, and m6.
# These can be any models you like, not just the ones shown here.

m3 <- occu(~ 1 + DOY_scaled ~ 1, data = umf) 
m4 <- occu(~ 1 + days_scaled ~ 1, data = umf)
m5 <- occu(~ 1 + DOY_scaled + days_scaled ~ 1, data = umf)
# whats the difference between this and not including the ones after the ~ (like above)?
# and why don't you include any occurrence covariates in these models?
# how do you choose which models to include?
# random effects like this?: 
rand <- occu(~ DOY_scaled + days_scaled ~ Humans + Development + Habitat + (1 | Array) + (1 + Year), data = umf)
rand <- occu(~ DOY_scaled ~ Habitat + (1 | Array), data = umf)

m6 <- occu(~ 1 ~ 1 + Humans + Development + Habitat, data = umf)
# heres one with the occurrence instead of detection covariates

# Now let's compare model fit of these models.
# First we need to create a list of the model outputs so they can be compared
# You can name the models if you like e.g. fitList('P(.)Psi(.)'=m1, 'P(wind)Psi(.)'=m2, .........
# or just use your original model names. If you do not provide model names you will
# get a warning message but you can ignore it if you wish.

# Just for fun let's fit a no-intercept model using our binary observer covariates
# to see if we can model detection as a function of temperature for just one
# observer

m7 <- occu(~ -1 + FredObs + OtherObs + FredObs:temp ~ 1, data=umf)
summary(m7)
# I don't understand this but I don't think I need to

# We use unmarked's fitList() function to create a list of models that we can then 
# compare using AIC

ms <- fitList(m1,m2,m3,m4,m5,m6)

# We can then use unmarked's modSel() function to create an AICc table

modSel(ms) # does this mean m6 is the best? since it has the lowest AIC? 
    # and what do all the parts of the model mean?
# Warning message:
    # In sqrt(diag(vcov(x, altNames = TRUE))) : NaNs produced

# Alternatively, we could use the aictab() function in the AICcmodavg package. 
# aictab() requires a list but modSel() does not return a list. Fortunately, we
# can extract a list object from our modSel() object as follows.

aictab(ms@fits)

# Notice that aictab() returns the AICc values by default (you can have it
# return AIC, QAIC, QAICc) as well as the log-likelihood values.

aictab(ms@fits, c.hat = 1.7) # do you need to set c hat to the value you got before?

# Notice how our model uncertainty just increased when using QAICc

comment
# tbh I skipped this whole next section because I'm not sure how it applies to me
    # (averaging over different observers etc)

# Model-averaged predicted values -------------


# We can also calculate model-averaged predicted occupancy and detection just
# as we did for our survival models in RMark. The big difference is that RMark contains
# its own functions for model selection and averaging. unmarked does not, with the
# important exception of the modSel() function. There are two primary packages for
# making multi-model inference, AICcmodavg and MuMIn. We will illustrate the calculation
# of model-averaged predictions using the AICcmodavg package.

# Let's first predict detection as a function of temperature across all of our models. We will
# use the modavgPred() function. This works in much the same way as the predict() function
# we just used, only we supply a list object containing the models over which we wish
# to average. We can also supply a vector of model names, although as with modSel() this
# is not necessary. We do need to supply a data frame of covariate values and specify
# the parameter for which we wish to make predictions ("det" or "state" for the
# single-season occupancy model). We can also include c-hat so that the standard
# errors of our predictions will be adjusted accordingly.

# We will need to create a new newdata= data frame containing values of every
# observation covariate included within our model list. But because we z-score
# standardized all our continuous covariates we can represent wind as 0 
# which will mean predicting detection for a given value of temperature and the mean
# wind speed But what about observer? We have four observers so let's predict
# the relationship between detection and temperature for each observer. We can use
# the expand.grid() function to create our newdata= data frame as follows.


temp_values <- seq(min(as.numeric(as.matrix(umf@obsCovs$temp)),na.rm=T),
                   max(as.numeric(as.matrix(umf@obsCovs$temp)),na.rm=T),0.2)

all_new_data <- expand.grid(wind=0,
                            temp=temp_values,
                            obs=na.omit(unique(umf@obsCovs$obs))
)

all_new_data
# Notice how we now have a range of temperature values for each observer

# Remember how we had our model with FredObs and OtherObs? That adds a new level
# of complexity when calculating model-averaged predicted values so let's re-make
# our model list without that model. This time we will make a named list.

mod_list <- list("Null"=m1,"wind"=m2,"obs"=m3,"temp"=m4,
                 "temp_wind"=m5,"temp_wind_obs"=m6)

det_pred_MA <- modavgPred(cand.set=mod_list,
                          modnames=names(mod_list), 
                          newdata=all_new_data,
                          c.hat=1.6,
                          parm.type=c("detect"))

# Note that this may take a while because the function is calculating predicted
# values for each model and then averaging them. So increasing the number of 
# values at which to predict and the number of models will both increase the length
# of time required to run this function.

# We now have a list containing model-averaged predicted probabilities of detection
# and their 95% CI. Let's plot them and because we will have different curves for
# each observer let's use ggplot() to plot them. We will not go into the details
# of the script to create this plot but I provide it as a template for your future use.

plot_data <- data.frame(est=det_pred_MA$mod.avg.pred, # create dataframe to plot
                        lower=det_pred_MA$lower.CL,
                        upper=det_pred_MA$upper.CL,
                        temp=all_new_data$temp,
                        obs=all_new_data$obs)

ModAvg_Temp_plot <- ggplot(data=plot_data, # create plot
                           aes(x=temp))+
  geom_ribbon(aes(ymin=lower,ymax=upper,fill=obs),alpha=0.1)+
  geom_line(aes(y=est,colour=obs),lwd=0.8)+
  scale_y_continuous("Probability of detection (p)",limits=c(0,1))+
  xlab(expression(paste("Air temperature (", degree,"C)")))+
  theme_bw(base_size = 10)
ModAvg_Temp_plot # view plot

# What happens if we had no over-dispersion and c.hat = 1?

det_pred_MA_chat1 <- modavgPred(cand.set=mod_list,
                                modnames=names(mod_list), 
                                newdata=all_new_data,
                                c.hat=1,
                                parm.type=c("detect"))

plot_data_chat1 <- data.frame(est=det_pred_MA_chat1$mod.avg.pred,
                              lower=det_pred_MA_chat1$lower.CL,
                              upper=det_pred_MA_chat1$upper.CL,
                              temp=all_new_data$temp,
                              obs=all_new_data$obs)

ModAvg_Temp_plot_chat1 <- ggplot(data=plot_data_chat1,
                                 aes(x=temp))+
  geom_ribbon(aes(ymin=lower,ymax=upper,fill=obs),alpha=0.1)+
  geom_line(aes(y=est,colour=obs),lwd=0.8)+
  scale_y_continuous("Probability of detection (p)",limits=c(0,1))+
  xlab(expression(paste("Air temperature (", degree,"C)")))+
  theme_bw(base_size = 10)
ModAvg_Temp_plot_chat1

# # Multi-Season (Dynamic) Occupancy Models ---------------- 
# DOES NOT APPLY TO ME!! Multiseason occupancy models are for MULTIPLE seasons of 
# the SAME location(s)!!
# 
# # Now let's build off of the single-season occupancy model and move on to the
# # multi-season or dynamic occupancy model. Data for this model follows a robust design
# # where you have T primary periods (e.g., years or seasons) and J secondary sampling
# # occasions (i.e., repeated surveys within a season) across N sites. Sites are assumed 
# # to be closed with regards to occupancy within each primary period but occupancy status
# # may change between primary periods due to colonization or extinction. Perhaps it's not 
# # too surprising that the unmarked function for fitting multi-season occupancy models
# # is called colext().
# 
# # Let's simulate some data to illustrate how to use the colext() function. 
# 
# # i don't understand this whole part. should I do this instead of using years as a random effect?
# # do you need to know colonization and extinction information to do this?
# Sites <- 1720 # number of sites
# Prim <- 5 # number of primary periods (for me, this'll be number of years)
# Sec <- 18 # number of secondary periods, repeats in primary (number of detection intervals)
# psi <- rep(NA, Prim) # Create occupancy
# dummyData <- rep(NA, Prim) # dummy placeholder for later use
# actualData <- array(dim = c(Sites, Prim)) # We'll fill this with "actual" data
# det <- array(NA, dim = c(Sites, Sec, Prim)) # This will be for our detection
# psi[1] <- 0.4 # initial occupancy
# p <- c(0.3, 0.5, 0.7, 0.3, 0.2) # detection probability in each primary session
# phi <- runif(n = Prim-1, min = 0.6, max = 0.8) # uniform random site fidelity probability (1-epsilon)
# gamma <- runif(n = Prim-1, min = 0.05, max = 0.2) # colonization probability
# actualData[, 1] <- rbinom(Sites, 1, psi[1]) # create initial occupancy
# 
# for(i in 1:Sites){ # loop through sites to fill in actual data
#   for(j in 2:Prim){ # loop through primary occasions to fill in actual data and expected
#     dummyData[j] <- actualData[i, j - 1]* phi[j - 1] + (1 - actualData[i, j - 1]) * gamma[j - 1] ## what is the expected occupancy for each site
#     actualData[i, j] <- rbinom(1, 1, dummyData[j]) ## what does the data actually look like
#   }
# }
# 
# for(i in 1:Sites){ ## loop through sites
#   for(j in 1:Prim){ ## loop through primary periods
#     prob <- actualData[i, j] * p[j] ## determine detection probability
#     for(k in 1:Sec){ ## loop through secondary periods
#       det[i, k, j] <- rbinom(1, 1, prob) ## getting observed data by incorporating detection
#     }
#   }
# }
# 
# #Let's see what these data look like by comparing actual data with observed data
# {plot(1:Prim, colMeans(actualData), type = "b", xlab = "Primary Period",
#       ylab = "Proportion of sites occupied", col = "black", xlim = c(0.5, 5.5),
#       ylim = c(0.1,0.6), lwd = 2, lty = 1, frame.plot = FALSE, las = 1, pch = 16)}
# psi.app <- colMeans(apply(det, c(1, 3), max))
# lines(1:Prim, psi.app, type = "b", col = "blue", lty = 3, lwd = 2)
# legend(1, 0.6, c("truth", "observed"), col = c("black", "blue"), lty = c(1,3), pch = c(16,1))
# # ok so for me this comes out with the truth and observed lines almost identical (but not quite)
# # no matter what the data is. The last point is always just a tiny bit off
# 
# # That's kind of cool. Notice the effect of detection on the "trend" in observed occupancy 
# # but also in its relationship to "truth". If you're so inclined, try changing the input 
# # values and see what you get. Ok, we've simulated data and know what occupancy, 
# # colonization, and extinction should be, so let's try and estimate those parameters.
# 
# # First we need to rearrange some data and put the data into an unmarked data frame.
# # To use the colext() function we will use the unmarkedMultFrame(). Conceptually, 
# # unmarkedMultFrame() works in a similar manner as unmarkedFrameOccu(). But there
# # are a few extra arguments to accommodate the fact that we have primary and secondary
# # sampling occasions.
# 
# detection <- matrix(det, 
#                     Sites, 
#                     Prim * Sec) # rearranging the detection data so it's all in one matrix
# 
# Primary <- matrix(c("First", "Second", "Third", "Fourth", "Fifth"),
#                   nrow(detection), Prim, byrow=T) ## creating a covariate for each primary period
# 
# # let's add these data so unmarked can use them. Note we only have yearlySiteCovs, 
# # because these are our primary periods. But if we had site-level covariates or
# # observation-level covariates we would include them in siteCovs= and obsCovs=, respectively.
# 
# umfRD <- unmarkedMultFrame(y = detection, 
#                            obsCovs = , 
#                            siteCovs = , 
#                            yearlySiteCovs = list(primary = Primary),
#                            numPrimary = 5)
# # Let's view the data.
# 
# summary(umfRD)
# 
# # So how well does the dynamic occupancy model estimate our parameters of interest?
# # Notice that we no longer have a double formula. Each parameter has its own separate 
# # formula.
# 
# MS_null <- colext(psiformula = ~ 1, 
#                   gammaformula = ~ 1, 
#                   epsilonformula = ~ 1, 
#                   pformula = ~ 1, 
#                   data=umfRD)
# summary(MS_null)
# 
# # how about confidence intervals on the back-transformed estimates?
# 
# backTransform(MS_null, type = "psi")
# confint(backTransform(MS_null, type = "psi"))
# 
# # how does this compare to the actual data (your graph)?
# psi # is this supposed to say "NA" for the last four?
# # What about our estimates for extinction, colonization, and detection?
# 
# backTransform(MS_null, type = "ext") # Extinction
# backTransform(MS_null, type = "col") # Colonization
# backTransform(MS_null, type = "det") # Detection
# 
# # However, remember that we simulated detection to vary by primary period. So our model
# # with constant detection actually does not reflect our data-generating mechanism.
# # Let's refit our model with year-specific detection probability. 
# 
# MS_year_p <- colext(psiformula = ~ 1, 
#                     gammaformula = ~ 1, 
#                     epsilonformula = ~ 1, 
#                     pformula = ~ primary, 
#                     data=umfRD)
# 
# # Compare with AICc
# 
# AICc(MS_null)
# AICc(MS_year_p)
# 
# # Plot our predicted detection probabilities against the true values
# 
# new_data <- data.frame(primary=as.vector(as.matrix(unique(umfRD@yearlySiteCovs))))
# new_data <- predict(MS_year_p, type = 'det', newdata = new_data, appendData=TRUE)
# new_data$Primary <- seq(1,nrow(new_data))
# head(new_data)
# 
# plot(Predicted~Primary, new_data,ylim=c(0,1),pch=16,
#      ylab="Probability of Detection",xlab="Primary Period")
# arrows(as.numeric(new_data$Primary),new_data$lower,
#        as.numeric(new_data$Primary),new_data$upper,
#        code=3,angle=90,length=0.1)
# points(as.numeric(new_data$Primary),p,pch=21,col="red",cex=2)
# legend('topright',legend=c("True","Estimated"),pch=c(21,16),
#        col=c("red","black"))
# 
# # Estimating GOF for a multi-season occupancy model -------------
# 
# # We can use the same mb.gof.test() to test for GOF for a multi-season occupancy model.
# # If we test for GOF using our model with yearly-specific detection rates, would we expect
# # to see any lack of fit? 
# 
# # Why or why not?
# 
# system.time(MS_occu_GOF <- mb.gof.test(MS_year_p, nsim = 100))
# # not sure what this is all about but it just runs for a while (10 minutes at least)
# 
# MS_occu_GOF
# 
# 
# 
# comment
# # I did not include the n mixture stuff because I don't think I'm looking for abundance
# # so I cut off the whole end of the code you gave me
