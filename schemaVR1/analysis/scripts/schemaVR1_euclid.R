# Script to run analysis of recall euclidean distance for schemaVR1
# Version 1.2
# Date:  13/02/2021
# Author: Joern Alexander Quent
# /* 
# ----------------------------- Libraries, settings and functions ---------------------------
# */
# Setting seed3
seed <- 7246
set.seed(seed)
seeds <- sample(1:9999, 2)

# Libaries
library(assortedRFunctions)
library(brms)

# General settings
cores2use <- 4

# /* 
# ----------------------------- Preparing data ---------------------------
# */
# Loading data
load("C:/Users/aq01/Desktop/schemaVR/schemaVR1/data/dataSchemaVR1_cleaned.RData")

# Scaling based on https://github.com/stan-dev/stan/wiki/Prior-Choice-Recommendations
# Mean = 0 and SD = 1
dataSchemaVR1_recall$Exp  <- dataSchemaVR1_recall$objLocTargetRating 
dataSchemaVR1_recall$sExp <- scale(dataSchemaVR1_recall$Exp)

# /* 
# ----------------------------- Model ---------------------------
# */
# Based on https://github.com/stan-dev/stan/wiki/Prior-Choice-Recommendations
prior_schemaVR1  <- c(prior(normal(0, 1) , class = "Intercept"),
                      prior(normal(0, 1) , class = "b")) 

model_schemaVR1_euclid1 <- brm(euclideanDist ~ sExp +  
                             I(sExp*sExp) +
                             (sExp + I(sExp*sExp) | subNum) +
                             (sExp + I(sExp*sExp) | objNum),
                           data = dataSchemaVR1_recall,
                           prior = prior_schemaVR1,
                           family = Gamma(link = "log"),
                           chains = 8,
                           warmup = 2000,
                           iter   = 16000,
                           cores = cores2use,
                           save_all_pars = TRUE,
                           sample_prior = TRUE,
                           seed = seeds[1],
                           control = list(adapt_delta = 0.95)) 

model_schemaVR1_euclid2 <- brm(euclideanDist ~ sExp +  
                                I(sExp*sExp) +
                                (1 | subNum) +
                                (1 | objNum),
                              data = dataSchemaVR1_recall,
                              prior = prior_schemaVR1,
                              family = Gamma(link = "log"),
                              chains = 8,
                              warmup = 2000,
                              iter   = 16000,
                              cores = cores2use,
                              save_all_pars = TRUE,
                              sample_prior = TRUE,
                              seed = seeds[2]) 

BF_randomEffect <- bayes_factor(model_schemaVR1_euclid1, model_schemaVR1_euclid2)


# /* 
# ----------------------------- Saving iamge ---------------------------
# */
save.image(datedFileNam('schemaVR1_euclid', '.RData'))