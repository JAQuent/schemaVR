# Script to run analysis of recall euclidean distance for schemaVR2
# Version 1.1
# Date:  15/09/2019
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
load("U:/Projects/schemaVR/schemaVR2/data/dataSchemaVR2_cleaned.RData")
load("U:/Projects/schemaVR/schemaVR1/analysis/schemaVR1_euclid_20191011_160302.RData")

# Scaling based on https://github.com/stan-dev/stan/wiki/Prior-Choice-Recommendations
# Mean = 0 and SD = 1
dataSchemaVR2_recall$Exp  <- dataSchemaVR2_recall$objLocTargetRating 
dataSchemaVR2_recall$sExp <- scale(dataSchemaVR2_recall$Exp)


# /* 
# ----------------------------- Get family parameters for prior ---------------------------
# */
fixef_schemaVR1_euclid <- fixef(model_schemaVR1_euclid2)

prior_schemaVR2  <- c(set_prior(priorString_normal(fixef_schemaVR1_euclid[1, 1], fixef_schemaVR1_euclid[1, 2]), 
                                class = "Intercept"),
                      set_prior(priorString_normal(fixef_schemaVR1_euclid[2, 1], fixef_schemaVR1_euclid[2, 2]), 
                                class = "b", 
                                coef = "sExp"),
                      set_prior(priorString_normal(fixef_schemaVR1_euclid[3, 1], fixef_schemaVR1_euclid[3, 2]), 
                                class = "b", 
                                coef = "IsExpMUsExp"))


# /* 
# ----------------------------- Model ---------------------------
# */
model_schemaVR2_euclid <- brm(euclideanDist ~ sExp +  
                                I(sExp*sExp) +
                                (1 | subNum) +
                                (1 | objNum),
                              data = dataSchemaVR2_recall,
                              prior = prior_schemaVR2,
                              family = Gamma(link = "log"),
                              cores = cores2use,
                              save_all_pars = TRUE,
                              sample_prior = TRUE,
                              seed = seeds[2]) 

# /* 
# ----------------------------- Saving iamge ---------------------------
# */
save.image(datedFileNam('schemaVR2_euclid', '.RData'))