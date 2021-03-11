# Script to run analysis of Euclidean distance data for schemaVR4
# Version 1.0
# Date:  11/03/2019
# Author: Joern Alexander Quent
# /* 
# ----------------------------- Libraries, settings and functions ---------------------------
# */
# Setting seed
seed <- 21612323
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
load("C:/Users/aq01/Desktop/schemaVR/schemaVR4/data/dataSchemaVR4.RData")
# To save memory only load prior that is needed
prior_schemaVR4_euclid <- loadToEnv("C:/Users/aq01/Desktop/schemaVR/schemaVR4/priors_for_schemaVR4_20210219_135801.RData")[["prior_schemaVR4_euclid"]];

# Create recall df
dataSchemaVR4_recall <- subset(dataSchemaVR4, dataSchemaVR4$recallMemory != 0 | !is.na(dataSchemaVR4$recallMemory))

# Mean = 0 and SD = 1 (This is different than the scaling for the logistic models)
dataSchemaVR4_recall$Exp  <- dataSchemaVR4_recall$objLocTargetRating 
dataSchemaVR4_recall$sExp <- scale(dataSchemaVR4_recall$Exp)


# /* 
# ----------------------------- Model ---------------------------
# */
model_schemaVR4_euclid <- brm(euclideanDist ~ sExp +  
                                I(sExp*sExp) +
                                (1 | subNum) +
                                (1 | objNum),
                              data = dataSchemaVR4_recall,
                              prior = prior_schemaVR4_euclid,
                              family = Gamma(link = "log"),
                              chains = 8,
                              warmup = 2000,
                              iter   = 16000,
                              cores = cores2use,
                              save_all_pars = TRUE,
                              sample_prior = TRUE,
                              seed = seeds[2]) 

# Summary and beeeeep
summary(model_schemaVR4_euclid)
beep(8)

# /* 
# ----------------------------- Saving image ---------------------------
# */
save.image(datedFileNam('schemaVR4_euclid', '.RData'))