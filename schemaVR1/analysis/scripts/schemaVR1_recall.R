# Script to run analysis of recall accuracy data for schemaVR1
# Version 2.1
# Date:  18/02/2021
# Author: Joern Alexander Quent
# /* 
# ----------------------------- Libraries, settings and functions ---------------------------
# */
# Setting seed
seed <- 724323846
set.seed(seed)

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
dataSchemaVR1_recall$Exp  <- dataSchemaVR1_recall$objLocTargetRating 
dataSchemaVR1_recall$sExp <- (dataSchemaVR1_recall$Exp - mean(dataSchemaVR1_recall$Exp ))/(sd(dataSchemaVR1_recall$Exp)/0.5)


# /* 
# ----------------------------- Model ---------------------------
# */
# Based on Gelman et al. (2008) and https://github.com/stan-dev/stan/wiki/Prior-Choice-Recommendations
prior_schemaVR1  <- c(prior(student_t(7, 0, 10) , class = "Intercept"),
                      prior(student_t(7, 0, 1) , class = "b")) 

model_schemaVR1_recall1 <- brm(accRecall ~ sExp +  
                             I(sExp*sExp) +
                             (sExp + I(sExp*sExp) | subNum) +
                             (sExp + I(sExp*sExp) | objNum),
                           data = dataSchemaVR1_recall,
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

model_schemaVR1_recall2 <- brm(accRecall ~ sExp +  
                                I(sExp*sExp) +
                                (1 | subNum) +
                                (1 | objNum),
                              data = dataSchemaVR1_recall,
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

BF_randomEffect <- bayes_factor(model_schemaVR1_recall1, model_schemaVR1_recall2)
# As BF favours the model with random intercepts only we choose this model for all subsequent steps

# /* 
# ----------------------------- Saving iamge ---------------------------
# */
save.image(datedFileNam('schemaVR1_recall', '.RData'))