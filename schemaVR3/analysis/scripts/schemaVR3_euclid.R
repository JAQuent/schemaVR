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
library(beepr)

# General settings
cores2use <- 4

# /* 
# ----------------------------- Preparing data ---------------------------
# */
# Loading data
load("C:/Users/aq01/Desktop/schemaVR/schemaVR3/data/dataSchemaVR3_cleaned.RData")
load("C:/Users/aq01/Desktop/schemaVR/schemaVR2/analysis/schemaVR2_euclid_20210213_141228.RData")

# Scaling based on https://github.com/stan-dev/stan/wiki/Prior-Choice-Recommendations
# Mean = 0 and SD = 1
dataSchemaVR3_recall$Exp  <- dataSchemaVR3_recall$objLocTargetRating 
dataSchemaVR3_recall$sExp <- scale(dataSchemaVR3_recall$Exp)

# /* 
# ----------------------------- Get family parameters for prior ---------------------------
# */
postDists                 <- posterior_samples(model_schemaVR2_euclid)
intercept_schemaVR3_euclid  <- brm(b_Intercept ~ 1,
                                   data = postDists,
                                   cores = cores2use,
                                   family = student(link = "identity", link_sigma = "log", link_nu = "logm1"))
b_sExp_schemaVR3_euclid <- brm(b_sExp ~ 1,
                               data = postDists,
                               cores = cores2use,
                               family = student(link = "identity", link_sigma = "log", link_nu = "logm1"))
b_IsExpMUsExp_schemaVR3_euclid <- brm(b_IsExpMUsExp ~ 1,
                                      data = postDists,
                                      cores = cores2use,
                                      family = student(link = "identity", link_sigma = "log", link_nu = "logm1"))
beep(8)
pp_check(b_IsExpMUsExp_schemaVR3_euclid)


prior_schemaVR3  <- c(set_prior(priorString_student(intercept_schemaVR3_euclid), 
                                class = "Intercept"),
                      set_prior(priorString_student(b_sExp_schemaVR3_euclid), 
                                class = "b", 
                                coef = "sExp"),
                      set_prior(priorString_student(b_IsExpMUsExp_schemaVR3_euclid), 
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
                              chains = 8,
                              warmup = 2000,
                              iter   = 16000,
                              cores = cores2use,
                              save_all_pars = TRUE,
                              sample_prior = TRUE,
                              seed = seeds[2]) 

summary(model_schemaVR3_euclid)
beep(8)

# /* 
# ----------------------------- Saving image ---------------------------
# */
save.image(datedFileNam('schemaVR3_euclid', '.RData'))