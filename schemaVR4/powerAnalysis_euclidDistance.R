# Script runs a power analysis for recall without experiment as an random effect
# Version 1.1
# Date: 15/09/2019
# Author: Joern Alexander Quent
# /* 
# ----------------------------- Libraries, settings and functions ---------------------------
# */
# Libraries
library(brms)
library(assortedRFunctions)
library(polspline)

# Seeds
seed <- 482584888
set.seed(seed)

# Functions
genDat_gamma_euclid <- function(numObj, numSub, shape, gamma00, beta1, beta2, w1_SD, w2_SD, u01_SD, u02_SD){
  # General values and variables need for the for loop.
  N      <- numObj * numSub
  x      <- scale(runif(N, min = -100, max = 100))
  z      <- c()
  index  <- 1
  
  # Coefficients
  gamma01 <- 1
  gamma02 <- 1
  
  # Varying intecepts
  w1      <- rnorm(numObj, mean = 0, sd = w1_SD)
  w2      <- rnorm(numSub, mean = 0, sd = w2_SD)
  u01     <- rnorm(numObj, mean = 0, sd = u01_SD)
  u02     <- rnorm(numSub, mean = 0, sd = u01_SD)
  
  # Generating dataset
  for(j in 1:numSub){
    for(i in 1:numObj){
      z[index] <- gamma00 + gamma01*w1[i] + u01[i] + gamma02*w2[j] + u02[j] + beta1*x[index] + beta2*x[index]*x[index]
      index <- index + 1
    } 
  }
  
  # Link errors to gamma distribution
  z <- exp(z)
  y <- rgamma(N, rate = shape / z, shape = shape)
  
  # Binding to data frame
  data.frame(euclideanDist = y,
             objLocTargetRating = x,
             subNum = factor(rep(1:numSub, each = numObj)),
             objNum = factor(rep(1:numObj, numSub)))
}


# /* 
# ----------------------------- Load data ---------------------------
# */
load('bayesianMM_euclidDist_20190914_175201.RData')

# /* 
# ----------------------------- Simulation ---------------------------
# */
# Simulation paramers
nSim      <- 100 # must be over 1
seeds     <- sample(9999999, nSim)
cores2use <- 4

# Getting priors
combinedData_fixef  <- fixef(combinedData_model)
combinedData_priors <- c(set_prior(priorString_normal(combinedData_fixef[1, 1], combinedData_fixef[1, 2]),
                                   class = "Intercept"),
                         set_prior(priorString_normal(combinedData_fixef[2, 1], combinedData_fixef[2, 2]),
                                   class = "b",
                                   coef = "objLocTargetRating"),
                         set_prior(priorString_normal(combinedData_fixef[3, 1], combinedData_fixef[3, 2]),
                                   class = "b",
                                   coef = "IobjLocTargetRatingMUobjLocTargetRating"))

# Model parameters based on mean estimates of posterior distributions of previous experiments
postDistPreviousExp <-posterior_samples(combinedData_model)
numObj  <- 20
numSub  <- 72
shape   <- mean(postDistPreviousExp$shape)
gamma00 <- mean(postDistPreviousExp$b_Intercept)
beta1   <- mean(postDistPreviousExp$b_objLocTargetRating)
beta2   <- mean(postDistPreviousExp$b_IobjLocTargetRatingMUobjLocTargetRating)
w1_SD   <- sd(ranef(combinedData_model)$`objNum`[,1,1])
w2_SD   <- sd(ranef(combinedData_model)$`subNum`[,1,1])
u01_SD  <- mean(postDistPreviousExp$sd_objNum__Intercept)
u02_SD  <- mean(postDistPreviousExp$sd_subNum__Intercept)

# Empty variables
bfs1.1    <- c()
bfs1.2    <- c()
bfs2      <- c()
times     <- c()
postDists <- matrix(data = NA, nrow = nSim, ncol = 4000)

# First iteration of loop for compiling model
sim_euclid_data  <- genDat_gamma_euclid(numObj, numSub, shape, gamma00, beta1, beta2, w1_SD, w2_SD, u01_SD, u02_SD)
sim_euclid_model <- brm(euclideanDist ~ objLocTargetRating +
                          I(objLocTargetRating*objLocTargetRating) + 
                          (1 | subNum) +
                          (1 | objNum),
                        family = Gamma(link = "log"),
                        data  = sim_euclid_data,
                        prior = combinedData_priors,
                        cores = cores2use,
                        save_all_pars = TRUE,
                        sample_prior = TRUE,
                        seed = seeds[1],
                        control = list(adapt_delta = 0.95, max_treedepth = 15)) 

# Extracting posterior distribution
postDists[1, ] <- posterior_samples(sim_euclid_model)$b_IobjLocTargetRatingMUobjLocTargetRating

# Calculating Bayes Factors
# BRMS method: beta = 0 vs. beta != 0
bfs1.1[1]     <- hypothesis(sim_euclid_model, "IobjLocTargetRatingMUobjLocTargetRating = 0")$hypothesis$Evid.Ratio

# Wagenmakers et al. (2010) unrestricted method: beta = 0 vs. beta != 0
### Code pasted and adapted from Wagenmakers et al. (2010) found here: http://www.ejwagenmakers.com/papers.html
fit.posterior <- logspline(postDists[1, ])
posterior     <- dlogspline(0, fit.posterior) 
prior         <- dnorm(0 ,combinedData_fixef[3, 1], combinedData_fixef[3, 2])
bfs1.2[1]     <- posterior/prior

# Wagenmakers et al. (2010) order-restricted method 1 based on renormalisation: beta = 0 vs. beta > 0 
posterior     <- dlogspline(0, fit.posterior) 

# Normalising by areas of posterior and prior that are included in the intervall to get density of 1
areaPosterior <- sum(postDists[1, ] < 0)/length(postDists[1, ])
posterior.OR  <- posterior/areaPosterior    
areaPrior     <- integrate(dnorm, mean = combinedData_fixef[3, 1], sd = combinedData_fixef[3, 2], lower = -Inf, upper = 0, abs.tol = 0)$value 
prior.OR      <- dnorm(0 ,combinedData_fixef[3, 1], combinedData_fixef[3, 2])/areaPrior
bfs2[1]       <- posterior.OR/prior.OR 
####

# Time stamp
times[1]        <- as.character(Sys.time())
print(paste(paste('Finished ', 1,':', sep = ''), times[1]))

# Simulation loop
for(i in 2:nSim){
  sim_euclid_data  <- genDat_gamma_euclid(numObj, numSub, shape, gamma00, beta1, beta2, w1_SD, w2_SD, u01_SD, u02_SD)
  sim_euclid_model <- update(sim_euclid_model,
                             newdata = sim_euclid_data,
                             recompile = FALSE,
                             cores = cores2use,
                             save_all_pars = TRUE,
                             sample_prior = TRUE,
                             seed = seeds[i])
  
  # Extracting posterior distribution
  postDists[i, ] <- posterior_samples(sim_euclid_model)$b_IobjLocTargetRatingMUobjLocTargetRating
  
  # Calculating Bayes Factors
  # BRMS method: beta = 0 vs. beta != 0
  bfs1.1[i]     <- hypothesis(sim_euclid_model, "IobjLocTargetRatingMUobjLocTargetRating = 0")$hypothesis$Evid.Ratio
  
  # Wagenmakers et al. (2010) unrestricted method: beta = 0 vs. beta != 0
  ### Code pasted and adapted from Wagenmakers et al. (2010) found here: http://www.ejwagenmakers.com/papers.html
  fit.posterior <- logspline(postDists[i, ])
  posterior     <- dlogspline(0, fit.posterior) 
  prior         <- dnorm(0 ,combinedData_fixef[3, 1], combinedData_fixef[3, 2])
  bfs1.2[i]     <- posterior/prior
  
  # Wagenmakers et al. (2010) order-restricted method 1 based on renormalisation: beta = 0 vs. beta > 0 
  posterior     <- dlogspline(0, fit.posterior) 
  
  # Normalising by areas of posterior and prior that are included in the intervall to get density of 1
  areaPosterior <- sum(postDists[i, ] < 0)/length(postDists[i, ])
  posterior.OR  <- posterior/areaPosterior    
  areaPrior     <- integrate(dnorm, mean = combinedData_fixef[3, 1], sd = combinedData_fixef[3, 2], lower = -Inf, upper = 0, abs.tol = 0)$value 
  prior.OR      <- dnorm(0 ,combinedData_fixef[3, 1], combinedData_fixef[3, 2])/areaPrior
  bfs2[i]       <- posterior.OR/prior.OR 
  ####
  
  # Time stamp
  times[i]        <- as.character(Sys.time())
  print(paste(paste('Finished ', i,':', sep = ''), times[i]))
}

# /* 
# ----------------------------- Saving image ---------------------------
# */
save.image(datedFileNam('powerAnalysis_euclid', '.RData'))