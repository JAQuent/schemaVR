# Script runs a power analysis for recollection with experiment as an random effect
# Version 1.0
# Date: 26/09/2019
# Author: Joern Alexander Quent
# /* 
# ----------------------------- Libraries, settings and functions ---------------------------
# */
# Libraries
library(brms)
library(assortedRFunctions)
library(polspline)

# Seeds
seed <- 48258
set.seed(seed)


# This model is not modelled as mixed linear model because of the unconventional nesting
# Functions
genDat_normal_recollection <- function(numObj, normalSigma, beta0, beta1){
  # General values and variables need for the for loop.
  N      <- numObj
  x      <- scale(runif(numObj, min = -100, max = 100))

  # Generating dataset
  z <- beta0 + beta1*x
  
  # Link errors to normal distribution
  y <- rnorm(N, z, normalSigma) 
  
  # Binding to data frame
  data.frame(asinPR = y,
             objLocTargetRating = x,
             objNum = factor(1:numObj))
}


# /* 
# ----------------------------- Load data ---------------------------
# */
load('bayesianMM_recollection_20190926_074717.RData')

# /* 
# ----------------------------- Simulation ---------------------------
# */
# Simulation paramers
nSim      <- 100 # must be over 1
seeds     <- sample(9999999, nSim)
cores2use <- 4

# Getting priors
combinedData_fixef  <- fixef(combinedData_rec_AFC)
combinedData_priors <- c(set_prior(priorString_normal(combinedData_fixef[1, 1], combinedData_fixef[1, 2]),
                                   class = "Intercept"),
                         set_prior(priorString_normal(combinedData_fixef[2, 1], combinedData_fixef[2, 2]),
                                   class = "b",
                                   coef = "objLocTargetRating"))

# Model parameters based on mean estimates of posterior distributions of previous experiments
postDistPreviousExp <-posterior_samples(combinedData_rec_AFC)
numObj        <- 87 # as in schemaVR3
normalSigma   <- mean(postDistPreviousExp$sigma)
beta0         <- mean(postDistPreviousExp$b_Intercept)
beta1         <- mean(postDistPreviousExp$b_objLocTargetRating)

# Empty variables
bfs1.1    <- c()
bfs1.2    <- c()
bfs2      <- c()
times     <- c()
postDists <- matrix(data = NA, nrow = nSim, ncol = 4000)

# First iteration of loop for compiling model
sim_recollection_data  <- genDat_normal_recollection(numObj, normalSigma, beta0, beta1)
sim_recollection_model <- brm(asinPR ~ objLocTargetRating,
                        data = sim_recollection_data,
                        cores = cores2use,
                        save_all_pars = TRUE,
                        sample_prior = TRUE,
                        prior = combinedData_priors,
                        seed = seeds[1],
                        control = list(adapt_delta = 0.9999)) 

# Extracting posterior distribution
postDists[1, ] <- posterior_samples(sim_recollection_model)$b_objLocTargetRating

# Calculating Bayes Factors
# BRMS method: beta = 0 vs. beta != 0
bfs1.1[1]     <- hypothesis(sim_recollection_model, "objLocTargetRating = 0")$hypothesis$Evid.Ratio

# Wagenmakers et al. (2010) unrestricted method: beta = 0 vs. beta != 0
### Code pasted and adapted from Wagenmakers et al. (2010) found here: http://www.ejwagenmakers.com/papers.html
fit.posterior <- logspline(postDists[1, ])
posterior     <- dlogspline(0, fit.posterior) 
prior         <- dnorm(0 ,combinedData_fixef[2, 1], combinedData_fixef[2, 2])
bfs1.2[1]     <- posterior/prior

# Wagenmakers et al. (2010) order-restricted method 1 based on renormalisation: beta = 0 vs. beta > 0 
posterior     <- dlogspline(0, fit.posterior) 
# renormalize:
area          <- sum(postDists[1, ] < 0)/length(postDists[1, ])
posterior.OR  <- posterior/area
prior.OR      <- 2*dnorm(0 ,combinedData_fixef[2, 1], combinedData_fixef[2, 2])  
bfs2[1]       <- posterior.OR/prior.OR 
####

# Time stamp
times[1]        <- as.character(Sys.time())
print(paste(paste('Finished ', 1,':', sep = ''), times[1]))

# Simulation loop
for(i in 2:nSim){
  sim_recollection_data  <- genDat_normal_recollection(numObj, normalSigma, beta0, beta1)
  sim_recollection_model <- update(sim_recollection_model,
                                   newdata = sim_recollection_data,
                                   recompile = FALSE,
                                   cores = cores2use,
                                   save_all_pars = TRUE,
                                   sample_prior = TRUE,
                                   seed = seeds[i])
  
  # Extracting posterior distribution
  postDists[i, ] <- posterior_samples(sim_recollection_model)$b_objLocTargetRating
  
  # Calculating Bayes Factors
  # BRMS method: beta = 0 vs. beta != 0
  bfs1.1[i]     <- hypothesis(sim_recollection_model, "objLocTargetRating = 0")$hypothesis$Evid.Ratio
  
  # Wagenmakers et al. (2010) unrestricted method: beta = 0 vs. beta != 0
  ### Code pasted and adapted from Wagenmakers et al. (2010) found here: http://www.ejwagenmakers.com/papers.html
  fit.posterior <- logspline(postDists[i, ])
  posterior     <- dlogspline(0, fit.posterior) 
  prior         <- dnorm(0 ,combinedData_fixef[2, 1], combinedData_fixef[2, 2])
  bfs1.2[i]     <- posterior/prior
  
  # Wagenmakers et al. (2010) order-restricted method 1 based on renormalisation: beta = 0 vs. beta > 0 
  posterior     <- dlogspline(0, fit.posterior) 
  # renormalize:
  area          <- sum(postDists[i, ] < 0)/length(postDists[i, ])
  posterior.OR  <- posterior/area
  prior.OR      <- 2*dnorm(0 ,combinedData_fixef[2, 1], combinedData_fixef[2, 2])  
  bfs2[i]       <- posterior.OR/prior.OR 
  ####
  
  # Time stamp
  times[i]        <- as.character(Sys.time())
  print(paste(paste('Finished ', i,':', sep = ''), times[i]))
}

# /* 
# ----------------------------- Saving image ---------------------------
# */
save.image(datedFileNam('powerAnalysis_recollection', '.RData'))
