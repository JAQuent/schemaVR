# Script to run analysis of AFC accuracy data for schemaVR2
# Version 1.1
# Date:  18/10/2019
# Author: Joern Alexander Quent
# /* 
# ----------------------------- Libraries, settings and functions ---------------------------
# */
# Setting seed
seed <- 938316
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
load("C:/Users/aq01/Desktop/schemaVR/schemaVR2/data/dataSchemaVR2_cleaned.RData")
load("C:/Users/aq01/Desktop/schemaVR/schemaVR1/analysis/schemaVR1_AFC_20210218_133915.RData")

# Scaling based on Gelman et al. (2008) and https://github.com/stan-dev/stan/wiki/Prior-Choice-Recommendations
# Mean = 0 and SD = 0.5
dataSchemaVR2_AFC$Exp  <- dataSchemaVR2_AFC$objLocTargetRating 
dataSchemaVR2_AFC$sExp <- (dataSchemaVR2_AFC$Exp - mean(dataSchemaVR2_AFC$Exp))/(sd(dataSchemaVR2_AFC$Exp)/0.5)


# /* 
# ----------------------------- Get family parameters for prior ---------------------------
# */
postDists                 <- posterior_samples(model_schemaVR1_AFC2)
intercept_schemaVR2_AFC   <- brm(b_Intercept ~ 1,
                                 data = postDists,
                                 cores = cores2use,
                                 family = student(link = "identity", link_sigma = "log", link_nu = "logm1"))

# Check, beep and sleep for 10 sec
pp_check(intercept_schemaVR2_AFC)
beep(8)
Sys.sleep(10)


b_sExp_schemaVR2_AFC <- brm(b_sExp ~ 1,
                            data = postDists,
                            cores = cores2use,
                            family = student(link = "identity", link_sigma = "log", link_nu = "logm1"))

# Check, beep and sleep for 10 sec
pp_check(b_sExp_schemaVR2_AFC)
beep(8)
Sys.sleep(10)


b_IsExpMUsExp_schemaVR2_AFC <- brm(b_IsExpMUsExp ~ 1,
                                   data = postDists,
                                   cores = cores2use,
                                   family = student(link = "identity", link_sigma = "log", link_nu = "logm1"))

# Check, beep and sleep for 10 sec
pp_check(b_IsExpMUsExp_schemaVR2_AFC)
beep(8)
Sys.sleep(10)


prior_schemaVR2  <- c(set_prior(priorString_student(intercept_schemaVR2_AFC), 
                                class = "Intercept"),
                      set_prior(priorString_student(b_sExp_schemaVR2_AFC), 
                                class = "b", 
                                coef = "sExp"),
                      set_prior(priorString_student(b_IsExpMUsExp_schemaVR2_AFC), 
                                class = "b", 
                                coef = "IsExpMUsExp"))

# /* 
# ----------------------------- Model ---------------------------
# */
model_schemaVR2_AFC <- brm(accAFC ~ sExp +  
                                I(sExp*sExp) +
                                (1 | subNum) +
                                (1 | objNum),
                            data = dataSchemaVR2_AFC,
                            prior = prior_schemaVR2,
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
save.image(datedFileNam('schemaVR2_AFC', '.RData'))