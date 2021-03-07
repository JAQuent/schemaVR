# Script to run analysis of recall accuracy data for schemaVR2
# Version 2.0
# Date:  13/02/2021
# Author: Joern Alexander Quent
# /* 
# ----------------------------- Libraries, settings and functions ---------------------------
# */
# Setting seed
seed <- 12846
set.seed(seed)

# Libraries
library(assortedRFunctions)
library(brms)
library(beepr)

# General settings
cores2use <- 4

# /* 
# ----------------------------- Preparing data ---------------------------
# */
# Loading data
load("C:/Users/aq01/Desktop/schemaVR/schemaVR2/data/dataSchemaVR2_cleaned.RData")
load("C:/Users/aq01/Desktop/schemaVR/schemaVR1/analysis/schemaVR1_recall_20210218_145113.RData")

# Scaling based on Gelman et al. (2008) and https://github.com/stan-dev/stan/wiki/Prior-Choice-Recommendations
# Mean = 0 and SD = 0.5
dataSchemaVR2_recall$Exp  <- dataSchemaVR2_recall$objLocTargetRating 
dataSchemaVR2_recall$sExp <- (dataSchemaVR2_recall$Exp - mean(dataSchemaVR2_recall$Exp ))/(sd(dataSchemaVR2_recall$Exp)/0.5)


# /* 
# ----------------------------- Get family parameters for prior ---------------------------
# */
postDists                 <- posterior_samples(model_schemaVR1_recall2)
intercept_schemaVR_recall <- brm(b_Intercept ~ 1,
                                 data = postDists,
                                 cores = cores2use,
                                 family = student(link = "identity", link_sigma = "log", link_nu = "logm1"))

# Check, beep and sleep for 10 sec
pp_check(intercept_schemaVR_recall)
beep(8)
Sys.sleep(10)

b_sExp_schemaVR_recall <- brm(b_sExp ~ 1,
                              data = postDists,
                              cores = cores2use,
                              family = student(link = "identity", link_sigma = "log", link_nu = "logm1"))


# Check, beep and sleep for 10 sec
pp_check(b_sExp_schemaVR_recall)
beep(8)
Sys.sleep(10)

b_IsExpMUsExp_schemaVR_recall <- brm(b_IsExpMUsExp ~ 1,
                                 data = postDists,
                                 cores = cores2use,
                                 family = student(link = "identity", link_sigma = "log", link_nu = "logm1"))

# Check, beep and sleep for 10 sec
pp_check(b_IsExpMUsExp_schemaVR_recall)
beep(8)
Sys.sleep(10)


prior_schemaVR2  <- c(set_prior(priorString_student(intercept_schemaVR_recall), 
                                class = "Intercept"),
                      set_prior(priorString_student(b_sExp_schemaVR_recall), 
                                class = "b", 
                                coef = "sExp"),
                      set_prior(priorString_student(b_IsExpMUsExp_schemaVR_recall), 
                                class = "b", 
                                coef = "IsExpMUsExp"))

# /* 
# ----------------------------- Model ---------------------------
# */
model_schemaVR2_recall <- brm(accRecall ~ sExp +  
                                I(sExp*sExp) +
                                (1 | subNum) +
                                (1 | objNum),
                              data = dataSchemaVR2_recall,
                              prior = prior_schemaVR2,
                              family = bernoulli(),
                              chains = 8,
                              warmup = 2000,
                              iter   = 16000,
                              cores = cores2use,
                              seed = seed) 

## Summary and beep
summary(model_schemaVR2_recall)
beep(8)

# /* 
# ----------------------------- Saving image ---------------------------
# */
save.image(datedFileNam('schemaVR2_recall', '.RData'))
