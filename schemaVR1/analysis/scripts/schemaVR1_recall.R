# Script to run analysis of recall accuracy data for schemaVR1
# Version 1.1
# Date:  15/09/2019
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

# General settings
cores2use <- 4

# /* 
# ----------------------------- Preparing data ---------------------------
# */
# Loading data
load("U:/Projects/schemaVR/schemaVR1/data/dataSchemaVR1_cleaned.RData")

# Scaling based on Gelman et al. (2008) and https://github.com/stan-dev/stan/wiki/Prior-Choice-Recommendations
# Mean = 0 and SD = 0.5
dataSchemaVR1_recall$Exp  <- dataSchemaVR1_recall$objLocTargetRating 
dataSchemaVR1_recall$sExp <- (dataSchemaVR1_recall$Exp - mean(dataSchemaVR1_recall$Exp ))/(sd(dataSchemaVR1_recall$Exp)/0.5)


# /* 
# ----------------------------- Model ---------------------------
# */
#combinedData_priors <- c(prior(normal(0, 1), class = "Intercept"),
#                         prior(normal(0, 1), class = "b")) 
# Based on Gelman et al. (2008) and https://github.com/stan-dev/stan/wiki/Prior-Choice-Recommendations
prior_schemaVR1  <- c(prior(student_t(3, 0, 10) , class = "Intercept"),
                      prior(student_t(3, 0, 2.5) , class = "b")) 

model_schemaVR1_recall1 <- brm(accRecall ~ sExp +  
                             I(sExp*sExp) +
                             (sExp + I(sExp*sExp) | subNum) +
                             (sExp + I(sExp*sExp) | objNum),
                           data = dataSchemaVR1_recall,
                           prior = prior_schemaVR1,
                           family = bernoulli(),
                           cores = cores2use,
                           save_all_pars = TRUE,
                           sample_prior = TRUE,
                           seed = seed) 

model_schemaVR1_recall2 <- brm(accRecall ~ sExp +  
                                I(sExp*sExp) +
                                (1 | subNum) +
                                (1 | objNum),
                              data = dataSchemaVR1_recall,
                              prior = prior_schemaVR1,
                              family = bernoulli(),
                              cores = cores2use,
                              save_all_pars = TRUE,
                              sample_prior = TRUE,
                              seed = seed) 

BF_randomEffect <- bayes_factor(model_schemaVR1_recall1, model_schemaVR1_recall2)
# As BF favours the model with random intercepts only we choose this model for all subsequent steps

# /* 
# ----------------------------- Saving iamge ---------------------------
# */
save.image(datedFileNam('schemaVR1_recall', '.RData'))