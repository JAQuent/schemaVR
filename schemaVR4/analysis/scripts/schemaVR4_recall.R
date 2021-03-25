# Script to run analysis of Recall accuracy data for schemaVR4
# Version 1.0
# Date:  11/03/2019
# Author: Joern Alexander Quent
# /* 
# ----------------------------- Libraries, settings and functions ---------------------------
# */
# Setting seed
seed <- 11612323
set.seed(seed)
seeds <- sample(1:9999, 2)

# Libraries
library(assortedRFunctions)
library(brms)
library(beepr)
library(R.utils)

# General settings
cores2use <- 4

# Setting wd
setwd("C:/Users/aq01/Desktop/schemaVR/schemaVR4/analysis")

# /* 
# ----------------------------- Loading, preparing data and getting priors ---------------------------
# */
# Loading data
load("C:/Users/aq01/Desktop/schemaVR/schemaVR4/data/dataSchemaVR4_cleaned.RData")
# To save memory only load prior that is needed
prior_schemaVR4_recall <- loadToEnv("C:/Users/aq01/Desktop/schemaVR/schemaVR4/priors_for_schemaVR4_20210219_135801.RData")[["prior_schemaVR4_recall"]];

# Create recall df
dataSchemaVR4_recall <- subset(dataSchemaVR4, dataSchemaVR4$recallMemory != 0 | !is.na(dataSchemaVR4$recallMemory))


# Scaling based on Gelman et al. (2008) and https://github.com/stan-dev/stan/wiki/Prior-Choice-Recommendations
# Mean = 0 and SD = 0.5
dataSchemaVR4_recall$Exp  <- dataSchemaVR4_recall$objLocTargetRating 
dataSchemaVR4_recall$sExp <- (dataSchemaVR4_recall$Exp - mean(dataSchemaVR4_recall$Exp ))/(sd(dataSchemaVR4_recall$Exp)/0.5)


# /* 
# ----------------------------- Model ---------------------------
# */
model_schemaVR4_recall <- brm(accRecall ~ sExp +  
                                I(sExp*sExp) +
                                (1 | subNum) +
                                (1 | objNum),
                              data = dataSchemaVR4_recall,
                              prior = prior_schemaVR4_recall,
                              family = bernoulli(),
                              chains = 8,
                              warmup = 2000,
                              iter   = 16000,
                              cores = cores2use,
                              save_all_pars = TRUE,
                              sample_prior = TRUE,
                              seed = seed) 
# Summary and beeeep
summary(model_schemaVR4_recall)
beep(8)

# /* 
# ----------------------------- Saving image ---------------------------
# */
save.image(datedFileNam('schemaVR4_recall', '.RData'))