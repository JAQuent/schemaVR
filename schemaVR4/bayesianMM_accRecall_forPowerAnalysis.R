# Script to run analysis of recall data for power analsys without experiment as random factor
# Version 1.1
# Date:  15/09/2019
# Author: Joern Alexander Quent
# /* 
# ----------------------------- Libraries, settings and functions ---------------------------
# */
# Setting seed
seed <- 7243846
set.seed(seed)

# Libaries
library(assortedRFunctions)
library(brms)
library(lmerTest)

# General settings
cores2use <- 4

# /* 
# ----------------------------- Preparing data ---------------------------
# */
# Loading data
load("U:/Projects/schemaVR/schemaVR3/analysis/dataForBayesianReanalysis.RData") 

# Concatenating data
combinedData <- data.frame(accRecall = c(dataSchemaVR1_recall$accRecall,
                                         dataSchemaVR2_recall$accRecall,
                                         dataSchemaVR3_recall$accRecall),
                           objLocTargetRating = c(dataSchemaVR1_recall$objLocTargetRating,
                                                  dataSchemaVR2_recall$objLocTargetRating,
                                                  dataSchemaVR3_recall$objLocTargetRating),
                           subNum = c(dataSchemaVR1_recall$subNum,
                                      dataSchemaVR2_recall$subNum,
                                      dataSchemaVR3_recall$subNum + max(dataSchemaVR2_recall$subNum)), # important to make subNUm unique
                           objNum = c(dataSchemaVR1_recall$objNum,
                                      dataSchemaVR2_recall$objNum,
                                      dataSchemaVR3_recall$objNum),
                           experiment = c(rep('1', length(dataSchemaVR1_recall$accRecall)),
                                          rep('2', length(dataSchemaVR2_recall$accRecall)),
                                          rep('3', length(dataSchemaVR3_recall$accRecall))))

# /* 
# ----------------------------- Model ---------------------------
# */
combinedData_priors <- c(prior(normal(0, 1), class = "Intercept"),
                         prior(normal(0, 1), class = "b")) 

combinedData_model  <- brm(accRecall ~ objLocTargetRating +  
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
save.image(datedFileNam('bayesianMM_recall', '.RData'))
