# Script to run analysis of AFC accuracy data for schemaVR1
# Version 2.1
# Date:  18/02/2021
# Author: Joern Alexander Quent
# /* 
# ----------------------------- Libraries, settings and functions ---------------------------
# */
# Setting seed
seed <- 13846
set.seed(seed)
seeds <- sample(1:9999, 2)

# Libaries
library(assortedRFunctions)
library(brms)
library(beepr)

# General settings
cores2use <- 4

# /* 
# ----------------------------- Preparing data ---------------------------
# */
# Loading data
load("C:/Users/aq01/Desktop/schemaVR/schemaVR1/data/dataSchemaVR1_cleaned.RData")

# Scaling based on Gelman et al. (2008) and https://github.com/stan-dev/stan/wiki/Prior-Choice-Recommendations
# Mean = 0 and SD = 0.5
dataSchemaVR1_AFC$Exp  <- dataSchemaVR1_AFC$objLocTargetRating 
dataSchemaVR1_AFC$sExp <- (dataSchemaVR1_AFC$Exp - mean(dataSchemaVR1_AFC$Exp ))/(sd(dataSchemaVR1_AFC$Exp)/0.5)

# /* 
# ----------------------------- Model ---------------------------
# */
# Based on Gelman et al. (2008) and https://github.com/stan-dev/stan/wiki/Prior-Choice-Recommendations
prior_schemaVR1  <- c(prior(student_t(7, 0, 10) , class = "Intercept"),
                      prior(student_t(7, 0, 1) , class = "b")) 

model_schemaVR1_AFC1 <- brm(accAFC ~ sExp +  
                             I(sExp*sExp) +
                             (sExp + I(sExp*sExp) | subNum) +
                             (sExp + I(sExp*sExp) | objNum),
                           data = dataSchemaVR1_AFC,
                           prior = prior_schemaVR1,
                           family = bernoulli(),
                           chains = 8,
                           warmup = 2000,
                           iter   = 16000,
                           cores = cores2use,
                           save_all_pars = TRUE,
                           sample_prior = TRUE,
                           seed = seed) 
# Beep and sleep
beep(8)
Sys.sleep(10)

model_schemaVR1_AFC2 <- brm(accAFC ~ sExp +  
                                I(sExp*sExp) +
                                (1 | subNum) +
                                (1 | objNum),
                            data = dataSchemaVR1_AFC,
                            prior = prior_schemaVR1,
                            family = bernoulli(),
                            chains = 8,
                            warmup = 2000,
                            iter   = 16000,
                            cores = cores2use,
                            save_all_pars = TRUE,
                            sample_prior = TRUE,
                            seed = seed) 

# Beep and sleep
beep(8)
Sys.sleep(10)

BF_randomEffect <- bayes_factor(model_schemaVR1_AFC1, model_schemaVR1_AFC2)
# As BF favours the model with random intercepts only we choose this model for all subsequent steps

# /* 
# ----------------------------- Saving image ---------------------------
# */
save.image(datedFileNam('schemaVR1_AFC', '.RData'))