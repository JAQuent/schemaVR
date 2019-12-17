# Script to run analysis of recall euclidean distance for schemaVR3
# Version 1.0
# Date:  16/09/2019
# Author: Joern Alexander Quent
# /* 
# ----------------------------- Libraries, settings and functions ---------------------------
# */
# Setting seed3
seed <- 246
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
load("U:/Projects/schemaVR/schemaVR3/data/dataSchemaVR3_cleaned.RData")
load("U:/Projects/schemaVR/schemaVR2/analysis/schemaVR2_euclid_20191015_135827.RData")

# Scaling based on https://github.com/stan-dev/stan/wiki/Prior-Choice-Recommendations
# Mean = 0 and SD = 1
dataSchemaVR3_recall$Exp  <- dataSchemaVR3_recall$objLocTargetRating 
dataSchemaVR3_recall$sExp <- scale(dataSchemaVR3_recall$Exp)

# /* 
# ----------------------------- Get family parameters for prior ---------------------------
# */
fixef_schemaVR2_euclid <- fixef(model_schemaVR2_euclid)

prior_schemaVR3  <- c(set_prior(priorString_normal(fixef_schemaVR2_euclid[1, 1], fixef_schemaVR2_euclid[1, 2]), 
                                class = "Intercept"),
                      set_prior(priorString_normal(fixef_schemaVR2_euclid[2, 1], fixef_schemaVR2_euclid[2, 2]), 
                                class = "b", 
                                coef = "sExp"),
                      set_prior(priorString_normal(fixef_schemaVR2_euclid[3, 1], fixef_schemaVR2_euclid[3, 2]), 
                                class = "b", 
                                coef = "IsExpMUsExp"))


# /* 
# ----------------------------- Model ---------------------------
# */
model_schemaVR3_euclid <- brm(euclideanDist ~ sExp +  
                                I(sExp*sExp) +
                                (1 | subNum) +
                                (1 | objNum),
                              data = dataSchemaVR3_recall,
                              prior = prior_schemaVR3,
                              family = Gamma(link = "log"),
                              cores = cores2use,
                              save_all_pars = TRUE,
                              sample_prior = TRUE,
                              seed = seeds[2]) 

# /* 
# ----------------------------- Saving iamge ---------------------------
# */
save.image(datedFileNam('schemaVR3_euclid', '.RData'))