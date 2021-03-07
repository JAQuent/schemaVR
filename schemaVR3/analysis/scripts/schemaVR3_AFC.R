# Script to run analysis of AFC accuracy data for schemaVR3
# Version 1.0
# Date:  16/10/2019
# Author: Joern Alexander Quent
# /* 
# ----------------------------- Libraries, settings and functions ---------------------------
# */
# Setting seed
seed <- 138316
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
load("U:/Projects/schemaVR/schemaVR2/analysis/schemaVR2_AFC_20191015_143912.RData")

# Scaling based on Gelman et al. (2008) and https://github.com/stan-dev/stan/wiki/Prior-Choice-Recommendations
# Mean = 0 and SD = 0.5
dataSchemaVR3_AFC$Exp  <- dataSchemaVR3_AFC$objLocTargetRating 
dataSchemaVR3_AFC$sExp <- (dataSchemaVR3_AFC$Exp - mean(dataSchemaVR3_AFC$Exp))/(sd(dataSchemaVR3_AFC$Exp)/0.5)

# /* 
# ----------------------------- Get family parameters for prior ---------------------------
# */
postDists                 <- posterior_samples(model_schemaVR2_AFC)
intercept_schemaVR2_AFC   <- brm(b_Intercept ~ 1,
                                 data = postDists,
                                 family = student(link = "identity", link_sigma = "log", link_nu = "logm1"))
b_sExp_schemaVR2_AFC <- brm(b_sExp ~ 1,
                              data = postDists,
                              family = student(link = "identity", link_sigma = "log", link_nu = "logm1"))
b_IsExpMUsExp_schemaVR2_AFC <- brm(b_IsExpMUsExp ~ 1,
                                     data = postDists,
                                     family = student(link = "identity", link_sigma = "log", link_nu = "logm1"))


prior_schemaVR3  <- c(set_prior(priorString_student(intercept_schemaVR2_AFC), 
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
model_schemaVR3_AFC <- brm(accAFC ~ sExp +  
                                I(sExp*sExp) +
                                (1 | subNum) +
                                (1 | objNum),
                              data = dataSchemaVR3_AFC,
                              prior = prior_schemaVR3,
                              family = bernoulli(),
                              cores = cores2use,
                              save_all_pars = TRUE,
                              sample_prior = TRUE,
                              seed = seed) 

# /* 
# ----------------------------- Saving iamge ---------------------------
# */
save.image(datedFileNam('schemaVR3_AFC', '.RData'))