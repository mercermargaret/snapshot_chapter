rm(list=ls()) # clean workspace.  Caution!!!!

library(unmarked)
library(AICcmodavg)
library(ggplot2)

# Single-Season Occupancy Models -----------------
# Read in a csv file of our data --------

# Our encounter histories are represented by columns visit1, visit2, visit3. One nice
# feature of unmarked is that we don't have to concatenate our encounter histories 
# like we do in RMark! We then have two site-level covariates, Native and PatchSize.
# We then have three survey-level covariates as denote by their 1, 2, 3 postscripts.
# Remember for survey-level covariates we need one covariate (i.e., column) per survey/visit.
# Also note that Obs1, Obs2, and Obs3 are 4-level categorical factors. 

data <- read.csv("data/UMoccupancy_v2.csv")
str(data)
head(data)

# Let's z-score standardize our continuous covariates as this is generally a good idea
# when fitting any kind of hierarchical model to help facilitate convergence
data$Native <- scale(data$Native)
data$PatchSize <- scale(data$PatchSize)
data[,grep("temp",colnames(data))] <- scale(data[,grep("temp",colnames(data))])
data[,grep("wind",colnames(data))] <- scale(data[,grep("wind",colnames(data))])

# As a first step, let's pull out the data that will inform each of the following three 
# components of our input data for fitting models in unmarked:
visits <- data[,grep("VISIT",colnames(data),ignore.case = T)]
tail(visits)
#  Notice that we do not have three surveys for all sites. That is totally fine and
# unmarked can handle the missing data.

SiteCovs <- data[,c("Native","PatchSize")]

ObsCovs <- list(temp=data[,grep("temp",colnames(data),ignore.case = T)], 
                wind=data[,grep("wind",colnames(data),ignore.case = T)], 
                obs=data[,grep("obs",colnames(data),ignore.case = T)])

# But wait, what if we think that detection might vary according to the amount of native
# vegetation? Can we use the same covariate to model occupancy and detection? You bet.
# All we need to do is create a data frame of a given site covariate with number of columns
# equal to the maximum number of surveys. Each survey within a site would have the same
# value (e.g., Native doesn't change from survey to survey).
NativeObs <- data.frame(Native1=data$Native,Native2=data$Native,
                        Native3=data$Native)

# Also, let's select one of the observers to serve as a demonstration of how we can
# use binary covariates and no-intercept models to model interactions. Let's pick Fred
# because he has the most observations. Note that because we have three surveys we need three 
# binary covariates denoting whether or not Fred was the observer. If we wanted to 
# create binary covariates for all four observers we would need 4 x 3 columns total!
# Aren't you glad that unmarked can handle factors instead of requiring dummy covariates!

FredObs <- data.frame(Fred1=ifelse(data$Obs1=="Fred",1,0),
                      Fred2=ifelse(data$Obs2=="Fred",1,0),
                      Fred3=ifelse(data$Obs3=="Fred",1,0))

OtherObs <- data.frame(Fred1=ifelse(data$Obs1!="Fred",1,0),
                      Fred2=ifelse(data$Obs2!="Fred",1,0),
                      Fred3=ifelse(data$Obs3!="Fred",1,0))

ObsCovs <- list(temp=data[,grep("temp",colnames(data),ignore.case = T)], 
                wind=data[,grep("wind",colnames(data),ignore.case = T)], 
                obs=data[,grep("obs",colnames(data),ignore.case = T)],
                native=NativeObs,
                FredObs=FredObs,
                OtherObs=OtherObs)

# Create an object for model fitting -----------

# Each unmarked model-fitting function will have its own R object that the data will
# need to be placed into in order to fit the model. For example, in order to fit a 
# single-season occupancy model with the occu() function, we format our data as a
# unmarkedFrameOccu object using the unmarkedFrameOccu() function.
# When in doubt about what data input object to use, just use the ? to find the help
# page for your particular model and find the associated input data object.

javans_umf <- unmarkedFrameOccu(y = visits, # Encounter history, must be a data frame or matrix
                         siteCovs = SiteCovs, # Site covariates, must be a data frame
                         obsCovs = ObsCovs # Observer covariates, must be list of data frames or matrices
                         )

# Let's look at our data with the summary function
summary(javans_umf)

# Now fit some single-season occupancy models --------------

# Unlike RMark, all unmarked model fitting functions allow us to specify the formula
# directly into the function without requiring the formulas to be in a list.
# unmarked formulas will generally (always?) use double right-hand formulas. 
# In the case of occu, the order of the formula (from left to right) is
# detection and occupancy.
# Let's start with the dot model.

m1 <- occu(~1 ~1, data = javans_umf)
class(m1)

# Let's examine the output

summary(m1)

# Note that our estimates are on the logit scale. Just like RMark, we could use plogis()
# to manually calculate them. But notice what a pain it is to dig the beta
# estimates out of our model object!

plogis(m1@estimates@estimates$state@estimates)

# We can also use unmarked's backTransform() function to do this for us

backTransform(m1, 'state')
backTransform(m1, 'det')

# Now let's model detection as a function of wind

m2 <- occu(~wind ~ 1, data=javans_umf)

summary(m2)

# Let's see what detection is when wind is average, which would be 0 because we
# z-score standardized our covariates. Here's where we get a little tricky because 
# we need to combine estimates (intercept and wind effect). So we need to provide the 
# coefficients= argument both a value for the intercept and wind, so a vector of two
# numeric values. The linearComb() function will interpret this as 
# 1*Intercept + 0*wind. We can then place linearComb() within backTransform() to
# get our estimates on the real scale.

linearComb(m2, coefficients = c(1,0), type='det')
backTransform(linearComb(m2, coefficients = c(1,0), type='det'))

# We can also see how detection varies by wind by calculating predicted values
# using the predict() function.
# Just like when using predict() with any GLM or GLMM, we need to create a 
# newdata = object, that is, a data frame containing the covariate values at which we
# would like to predict detection (or occupancy)
# Because it is always a good idea to predict within the domain of our data 
# (i.e., not extrapolate beyond the covariate values we actually observed)
# let's create a sequence of wind values spanning the full range of our observed
# wind values.
# Note that if we wanted to predict for occupancy we would change type="det" to
# type="state" because occupancy is technically the state variable.

new_data <- data.frame(wind=seq(min(as.numeric(as.matrix(javans_umf@obsCovs$wind)),na.rm=T),
                                max(as.numeric(as.matrix(javans_umf@obsCovs$wind)),na.rm=T),0.1))
new_data <- predict(m2, type = 'det', newdata = new_data, appendData=TRUE)
head(new_data)

plot(Predicted~wind, new_data,ylim=c(0,1),type="l",lwd=3,
     ylab="Probability of Detection",xlab="Wind Speed")
lines(new_data$wind,new_data$lower,lty=3)
lines(new_data$wind,new_data$upper,lty=3)


# I'm gonna practice this and predict for an occupancy variable
m0 <- occu(~ 1 ~ Native, data=javans_umf)
new_data <- data.frame(Native=seq(min(as.numeric(as.matrix(javans_umf@siteCovs$Native)),na.rm=T),
                                max(as.numeric(as.matrix(javans_umf@siteCovs$Native)),na.rm=T),0.1))
new_data <- predict(m0, type = 'state', newdata = new_data, appendData=TRUE)
head(new_data)

plot(Predicted~Native, new_data,ylim=c(0,1),type="l",lwd=3,
     ylab="Probability of Occurrence",xlab="Native Plants")
lines(new_data$Native,new_data$lower,lty=3)
lines(new_data$Native,new_data$upper,lty=3)
# i got it!! it worked!!!!

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

global_mod <- occu(~temp + wind + obs + Native ~Native + PatchSize, data = javans_umf)

# The AICcmodavg package we loaded at the beginning
# of the lab contains a function that can run this test for us. 
# This is the mb.gof.test() function. We simply provide the model we wish to evaluate 

system.time(global_MB <- mb.gof.test(global_mod,nsim=10))

# Note that this takes about 11 seconds to run just 10 simulations. So 10,000
# simulations would require about 30 hours! Sounds like a great use for the computer
# over the weekend!
# The error messages are because we have missing observations in our data.

global_MB

# Fit and compare additional models ---------------

# Go ahead and fit 4 more models and call them m3, m4, m5, and m6.
# These can be any models you like, not just the ones shown here.

m3 <- occu(~1 + obs ~ 1, data=javans_umf) # whats the difference between this and not including the one before the + (like above)?
m4 <- occu(~1 + temp ~ 1, data=javans_umf)
m5 <- occu(~1 + temp + wind ~ 1, data=javans_umf)
m6 <- occu(~1 + temp + wind + obs ~ 1, data=javans_umf)

# Now let's compare model fit of these models.
# First we need to create a list of the model outputs so they can be compared
# You can name the models if you like e.g. fitList('P(.)Psi(.)'=m1, 'P(wind)Psi(.)'=m2, .........
# or just use your original model names. If you do not provide model names you will
# get a warning message but you can ignore it if you wish.

# Just for fun let's fit a no-intercept model using our binary observer covariates
# to see if we can model detection as a function of temperature for just one
# observer

m7 <- occu(~-1 + FredObs + OtherObs + FredObs:temp ~ 1, data=javans_umf)
summary(m7)

# We use unmarked's fitList() function to create a list of models that we can then 
# compare using AIC

ms <- fitList(m1,m2,m3,m4,m5,m6,m7)

# We can then use unmarked's modSel() function to create an AICc table

modSel(ms)

# Alternatively, we could use the aictab() function in the AICcmodavg package. 
# aictab() requires a list but modSel() does not return a list. Fortunately, we
# can extract a list object from our modSel() object as follows.

aictab(ms@fits)

# Notice that aictab() returns the AICc values by default (you can have it
# return AIC, QAIC, QAICc) as well as the log-likelihood values.

aictab(ms@fits, c.hat=1.7)

# Notice how our model uncertainty just increased when using QAICc
  
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

temp_values <- seq(min(as.numeric(as.matrix(javans_umf@obsCovs$temp)),na.rm=T),
                   max(as.numeric(as.matrix(javans_umf@obsCovs$temp)),na.rm=T),0.2)

all_new_data <- expand.grid(wind=0,
                            temp=temp_values,
                            obs=na.omit(unique(javans_umf@obsCovs$obs))
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

# What happens if we had no over-dispersion and c.hat=1?

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

# Multi-Season (Dynamic) Occupancy Models ----------------

# Now let's build off of the single-season occupancy model and move on to the
# multi-season or dynamic occupancy model. Data for this model follows a robust design
# where you have T primary periods (e.g., years or seasons) and J secondary sampling
# occasions (i.e., repeated surveys within a season) across N sites. Sites are assumed 
# to be closed with regards to occupancy within each primary period but occupancy status
# may change between primary periods due to colonization or extinction. Perhaps it's not 
# too surprising that the unmarked function for fitting multi-season occupancy models
# is called colext().

# Let's simulate some data to illustrate how to use the colext() function. 

# i don't understand this whole part
Sites <- 100 # number of sites
Prim <- 5 # number of primary periods (for me, this'll be number of years)
Sec <- 3 # number of secondary periods, repeats in primary (number of detection intervals)
psi <- rep(NA, Prim) # Create occupancy
dummyData <- rep(NA,Prim) # dummy placeholder for later use
actualData <- array(dim = c(Sites, Prim)) # We'll fill this with "actual" data
det <- array(NA, dim=c(Sites, Sec, Prim)) # This will be for our detection
psi[1] <- 0.4 # initial occupancy
p <- c(0.3, 0.5, 0.7, 0.3, 0.2) # detection probability in each primary session
phi <- runif(n=Prim-1,min=0.6, max=0.8) # uniform random site fidelity probability (1-epsilon)
gamma <- runif(n=Prim-1, min=0.05, max=0.2) # colonization probability
actualData[,1] <- rbinom(Sites, 1, psi[1]) # create initial occupancy
for(i in 1:Sites){ # loop through sites to fill in actual data
  for(j in 2:Prim){ # loop through primary occassions to fill in actual data and expected
    dummyData[j] <- actualData[i, j-1]*phi[j-1] + (1-actualData[i, j-1])*gamma[j-1] ## what is the expected occupancy for each site
    actualData[i,j] <- rbinom(1,1,dummyData[j]) ## what does the data actually look like
  }
}
for(i in 1:Sites){ ## loop through sites
  for(j in 1:Prim){ ## loop through primary periods
    prob <- actualData[i,j] * p[j] ## determine detection probability
    for(k in 1:Sec){ ## loop through secondary periods
      det[i,k,j] <- rbinom(1,1,prob) ## getting observed data by incorporating detection
    }
  }
}
#Let's see what these data look like by comparing actual data with observed data
{plot(1:Prim, colMeans(actualData), type = "b", xlab = "Primary Period",
      ylab = "Proportion of sites occupied", col = "black", xlim=c(0.5, 5.5),
      ylim = c(0.1,0.6), lwd = 2, lty = 1, frame.plot = FALSE, las = 1, pch=16)}
psi.app <- colMeans(apply(det, c(1,3), max))
lines(1:Prim, psi.app, type = "b", col = "blue", lty=3, lwd = 2)
legend(1, 0.6, c("truth", "observed"),col=c("black", "blue"), lty=c(1,3), pch=c(16,1))

# That's kind of cool. Notice the effect of detection on the "trend" in observed occupancy 
# but also in its relationship to "truth". If you're so inclined, try changing the input 
# values and see what you get. Ok, we've simulated data and know what occupancy and 
# colonization, and extinction should be, let's try and estimate those parameters.

# First we need to rearrange some data and put the data into an unmarked data frame.
# To use the colext() function we will use the unmarkedMultFrame(). Conceptually, 
# unmarkedMultFrame() works in a similar manner as unmarkedFrameOccu(). But there
# are a few extra arguments to accommodate the fact that we have primary and secondary
# sampling occasions.

detection <- matrix(det, 
                    Sites, 
                    Prim*Sec) # rearranging the detection data so it's all in one matrix

Primary <- matrix(c("First","Second","Third","Fourth","Fifth"),
                  nrow(detection),Prim,byrow=T) ## creating a covariate for each primary period

# let's add these data so unmarked can use them. Note we only have yearlySiteCovs, 
# because these are our primary periods. But if we had site-level covariates or
# observation-level covariates we would include them in siteCovs= and obsCovs=, respectively.

javans_umfRD <- unmarkedMultFrame(y=detection, 
                           obsCovs= , 
                           siteCovs= , 
                           yearlySiteCovs=list(primary=Primary),
                           numPrimary=5)
# Let's view the data.

summary(javans_umfRD)

# So how well does the dynamic occupancy model estimate our parameters of interest?
# Notice that we no longer have a double formula. Each parameter has its own separate 
# formula.

MS_null <- colext(psiformula = ~ 1, 
               gammaformula = ~ 1, 
               epsilonformula = ~ 1, 
               pformula = ~ 1, 
               data=javans_umfRD)
summary(MS_null)

# how about confidence intervals on the back-transformed estimates?

backTransform(MS_null,type="psi")
confint(backTransform(MS_null,type="psi"))

# how does this compare to the actual data (your graph)?
psi

# What about our estimates for extinction, colonization, and detection?

backTransform(MS_null,type="ext") # Extinction
backTransform(MS_null,type="col") # Colonization
backTransform(MS_null,type="det") # Detection

# However, remember that we simulated detection to vary by primary period. So our model
# with constant detection actually does not reflect our data-generating mechanism.
# Let's refit our model with year-specific detection probability. 

MS_year_p <- colext(psiformula = ~ 1, 
                  gammaformula = ~ 1, 
                  epsilonformula = ~ 1, 
                  pformula = ~ primary, 
                  data=javans_umfRD)

# Compare with AICc

AICc(MS_null)
AICc(MS_year_p)

# Plot our predicted detection probabilities against the true values

new_data <- data.frame(primary=as.vector(as.matrix(unique(javans_umfRD@yearlySiteCovs))))
new_data <- predict(MS_year_p, type = 'det', newdata = new_data, appendData=TRUE)
new_data$Primary <- seq(1,nrow(new_data))
head(new_data)

plot(Predicted~Primary, new_data,ylim=c(0,1),pch=16,
     ylab="Probability of Detection",xlab="Primary Period")
arrows(as.numeric(new_data$Primary),new_data$lower,
       as.numeric(new_data$Primary),new_data$upper,
       code=3,angle=90,length=0.1)
points(as.numeric(new_data$Primary),p,pch=21,col="red",cex=2)
legend('topright',legend=c("True","Estimated"),pch=c(21,16),
       col=c("red","black"))

# Estimating GOF for a multi-season occupancy model -------------

# We can use the same mb.gof.test() to test for GOF for a multi-season occupancy model.
# If we test for GOF using our model with yearly-specific detection rates, would we expect
# to see any lack of fit? 

# Why or why not?

system.time(MS_occu_GOF <- mb.gof.test(MS_year_p, nsim = 100))

MS_occu_GOF

# N-mixture abundance model ---------------------------- 
# so I won't use this because I don't care about abundance?

# Now let's fit some N-mixture models in unmarked. We are now estimating abundance 
# (rather than detection) as the state variable while still controlling for imperfect
# detection. 

# Let's read in the data -----------

kestrelData <- read.csv("FalconData.csv")

head(kestrelData)

# We have three repeat visits, two observation-level covariates (Day and Temp),
# and two site-level covariates (County and Point)

# As with the occupancy models, unmarked requires our data be in a specific
# format before fitting an N-mixture abundance model. Because we will fit our data
# using the pcount() function, we will use the unmarkedFramePCount() function
# to format our data.

kestrel  <- unmarkedFramePCount(y =kestrelData[,grep("Visit",colnames(kestrelData))], 
                                siteCovs = kestrelData[,c("County","Point")], 
                                obsCovs = list(day = kestrelData[,grep("Day",colnames(kestrelData))], 
                                               temp=kestrelData[,grep("Temp",colnames(kestrelData))])
                                )
summary(kestrel)

# If you are an old-timey biologist, you may be interested in an index of Kestrel abundance, 
# and you get average counts per point per visit. Let's do this using the apply function
# remembering that "1" means we are applying our function (the mean() function in this case)
# across rows ("2" would mean we are taking the mean across columns)

MeanCounts <- apply(kestrel@y,1,mean)

# and create histogram of the mean number of kestrels detected per point

hist(MeanCounts, col="gray", breaks=seq(0,6,1), main="Kestrel", xlab="Detections per point")

# Fit some N-mixture models --------

# Now let's fit some models assuming abundance has a Poisson distribution (N-mixture model) 
# The structure of pcount() has the detection process first and state variable second
# as with occu() but let's start with the null model

# Notice that we have a new argument, K=. K is the upper summation limit for the summation
# over the random effects of the integrated likelihood. The default is the observed
# maximum count (5 in this case) plus 100. So note that K=10 is much lower than we use
# just by default. But let's see what happens.

null <- pcount(~1 ~1, data = kestrel, K=10) 

# Yay! you ran your first N-Mixture model, Take a look at these estimates
null

# let's increase K and see if there is any change.

null2 <- pcount(~1 ~1, data = kestrel, K=100)
null2

# Were the estimates the same? What's going on? Here's a case where we aren't integrating 
# the maximum likelihood over the entire possible range, we need our estimates to stabilize 
# so let's try this once more.

null3 <- pcount(~1 ~1, data = kestrel, K=1000)
null3

# Note the difference in time it took to run the model. 
# However, also note there was no change in maximum likelihood estimator as indicated by AIC, 
# so you should be fine sticking with the smaller number.  However, for some really crazy 
# datasets, ones that are highly variable, you may find that you can't get this 
# thing to settle down. The negative binomial is especially problematic so 
# be very careful when running with that. It happens to be fairly unstable.

# Back to the data. Let's convert our estimates back to the original scale. 

# What will those scales be for each of our parameters?

# We can use backTransform() because we have no covariates

backTransform(null2, type="state")	
backTransform(null2, type="det")		

# Any ideas of how we could "manually" calculate these estimates using just our
# beta estimates?

# Notice how temperature is one of our observation-level covariates. What if we think that
# detection is lower at the extreme temperature (highs and lows)? Any ideas?

temp_quad <- pcount(~temp+I(temp^2) ~1, kestrel, K=50)

# We can predict detection across our observed range of temperatures to see what type
# of relationship we have.

new_data <- data.frame(temp=seq(min(as.numeric(as.matrix(kestrel@obsCovs$temp)),na.rm=T),
                                max(as.numeric(as.matrix(kestrel@obsCovs$temp)),na.rm=T),0.5))
new_data <- predict(temp_quad, type = 'det', newdata = new_data, appendData=TRUE)
head(new_data)

plot(Predicted~temp, new_data,ylim=c(0,1),type="l",lwd=3,
     ylab="Probability of Detection",xlab="Temperature")
lines(new_data$temp,new_data$lower,lty=3)
lines(new_data$temp,new_data$upper,lty=3)

# Now remember how we are assuming that our true (latent) counts follow a Poisson distribution?
# You may recall that the Poisson distribution assumes the mean is equal to the variance.
# Turns out this often not the case with real-world count data; typically the variance
# is greater than the mean which means that Poisson models often suffer from 
# over-dispersion (i.e., extra-Poisson variation). One alternative is to fit our
# N-mixture model with a Negative Binomial distribution. This can easily be done in 
# pcount() using the mixture= argument. You can model counts using either
# Poisson ("P"), Negative Binomial ("NB"), or Zero-Inflated Poisson ("ZIP")
# distributions.
# Try re-fitting the quadratic temperature model using the Negative Binomial
# and then use AICc to compare with the Poisson model.

temp_quad_NB <- pcount(~temp+I(temp^2) ~1, kestrel, K=50,
                       mixture="NB")

AICc(temp_quad)
AICc(temp_quad_NB)

# How about the parameter estimates? Are they the same?

summary(temp_quad)
summary(temp_quad_NB)

# Goodness-of-fit testing for N-mixture models -----------

# Just as with occupancy models, there is a function within the AICcmodavg package
# that can run a chi-square-based GOF test for N-mixture models. The conceptual basis
# behind this test is virtually the same as for occupancy models. So let's run some
# simulations and see what we get. 

Nmix_global <- pcount(~day + temp ~County, kestrel, K=100)

system.time(Nmix_GOF <- Nmix.gof.test(Nmix_global, nsim=10))

# Notice that these simulations also take some time.

Nmix_GOF

# Our p-value is highly non-significant and this is reflected by our low estimate
# for c-hat confirming good model fit and no evidence of over-dispersion.


            