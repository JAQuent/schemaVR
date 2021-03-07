# Script to run analysis of recall accuracy data for schemaVR3
# Version 2.1
# Date:  19/02/2021
# Author: Joern Alexander Quent
# /* 
# ----------------------------- Libraries, settings and functions ---------------------------
# */
# Setting seed
seed <- 12847
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
load("C:/Users/aq01/Desktop/schemaVR/schemaVR3/data/dataSchemaVR3_cleaned.RData")
load("C:/Users/aq01/Desktop/schemaVR/schemaVR2/analysis/schemaVR2_recall_20210218_182205.RData")

# Scaling based on Gelman et al. (2008) and https://github.com/stan-dev/stan/wiki/Prior-Choice-Recommendations
# Mean = 0 and SD = 0.5
dataSchemaVR3_recall$Exp  <- dataSchemaVR3_recall$objLocTargetRating 
dataSchemaVR3_recall$sExp <- (dataSchemaVR3_recall$Exp - mean(dataSchemaVR3_recall$Exp ))/(sd(dataSchemaVR3_recall$Exp)/0.5)


# /* 
# ----------------------------- Get family parameters for prior ---------------------------
# */
postDists                 <- posterior_samples(model_schemaVR2_recall)
intercept_schemaVR2_recall <- brm(b_Intercept ~ 1,
                                 data = postDists,
                                 cores = cores2use,
                                 family = student(link = "identity", link_sigma = "log", link_nu = "logm1"))

# Check, beep and sleep for 10 sec
pp_check(intercept_schemaVR2_recall)
beep(8)
Sys.sleep(10)

b_sExp_schemaVR2_recall <- brm(b_sExp ~ 1,
                               data = postDists,
                               cores = cores2use,
                               family = student(link = "identity", link_sigma = "log", link_nu = "logm1"))

# Check, beep and sleep for 10 sec
pp_check(b_sExp_schemaVR2_recall)
beep(8)
Sys.sleep(10)

b_IsExpMUsExp2_schemaVR2_recall <- brm(b_IsExpMUsExp ~ 1,
                                       data = postDists,
                                       cores = cores2use,
                                       family = student(link = "identity", link_sigma = "log", link_nu = "logm1"))

# Check, beep and sleep for 10 sec
pp_check(b_IsExpMUsExp2_schemaVR2_recall)
beep(8)
Sys.sleep(10)

prior_schemaVR3  <- c(set_prior(priorString_student(intercept_schemaVR2_recall), 
                                class = "Intercept"),
                      set_prior(priorString_student(b_sExp_schemaVR2_recall), 
                                class = "b", 
                                coef = "sExp"),
                      set_prior(priorString_student(b_IsExpMUsExp2_schemaVR2_recall), 
                                class = "b", 
                                coef = "IsExpMUsExp"))

# /* 
# ----------------------------- Model ---------------------------
# */
model_schemaVR3_recall <- brm(accRecall ~ sExp +  
                                I(sExp*sExp) +
                                (1 | subNum) +
                                (1 | objNum),
                              data = dataSchemaVR3_recall,
                              prior = prior_schemaVR3,
                              family = bernoulli(),
                              chains = 8,
                              warmup = 2000,
                              iter   = 16000,
                              cores = cores2use,
                              save_all_pars = TRUE,
                              sample_prior = TRUE,
                              seed = seed) 
# Beep
beep(8)

# /* 
# ----------------------------- Saving image ---------------------------
# */
save.image(datedFileNam('schemaVR3_recall', '.RData'))