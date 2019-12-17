# Script to run analysis of AFC data for power analsys without experiment as random factor
# Version 1.0
# Date:  15/09/2019
# Author: Joern Alexander Quent
# /* 
# ----------------------------- Libraries, settings and functions ---------------------------
# */
# Setting seed
seed <- 724383446
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
load("U:/Projects/schemaVR/schemaVR3/analysis/dataForBayesianReanalysis.RData") 

# Concatenating data
combinedData <- data.frame(accAFC = c(dataSchemaVR1_AFC$accAFC,
                                         dataSchemaVR2_AFC$accAFC,
                                         dataSchemaVR3_AFC$accAFC),
                           objLocTargetRating = c(dataSchemaVR1_AFC$objLocTargetRating,
                                                  dataSchemaVR2_AFC$objLocTargetRating,
                                                  dataSchemaVR3_AFC$objLocTargetRating),
                           subNum = c(dataSchemaVR1_AFC$subNum,
                                      dataSchemaVR2_AFC$subNum,
                                      dataSchemaVR3_AFC$subNum + max(dataSchemaVR2_AFC$subNum)), # important to make subNUm unique
                           objNum = c(dataSchemaVR1_AFC$objNum,
                                      dataSchemaVR2_AFC$objNum,
                                      dataSchemaVR3_AFC$objNum),
                           experiment = c(rep('1', length(dataSchemaVR1_AFC$accAFC)),
                                          rep('2', length(dataSchemaVR2_AFC$accAFC)),
                                          rep('3', length(dataSchemaVR3_AFC$accAFC))))

# /* 
# ----------------------------- Model ---------------------------
# */
combinedData_priors <- c(prior(normal(0, 1), class = "Intercept"),
                         prior(normal(0, 1), class = "b")) 

combinedData_model  <- brm(accAFC ~ objLocTargetRating +  
                             I(objLocTargetRating*objLocTargetRating) +
                             (1 | subNum) +
                             (1 | objNum),
                           data = combinedData,
                           family = bernoulli(),
                           prior = combinedData_priors,
                           cores = cores2use,
                           save_all_pars = TRUE,
                           sample_prior = TRUE,
                           seed = seed,
                           control = list(adapt_delta = 0.9999, max_treedepth = 15)) 

# /* 
# ----------------------------- Saving iamge ---------------------------
# */
save.image(datedFileNam('bayesianMM_AFC', '.RData'))
