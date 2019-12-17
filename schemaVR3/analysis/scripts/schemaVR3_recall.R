# Script to run analysis of recall accuracy data for schemaVR3
# Version 1.0
# Date:  16/10/2019
# Author: Joern Alexander Quent
# /* 
# ----------------------------- Libraries, settings and functions ---------------------------
# */
# Setting seed
seed <- 12847
set.seed(seed)

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
load("U:/Projects/schemaVR/schemaVR2/analysis/schemaVR2_recall_20191015_122558.RData")

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
                                 family = student(link = "identity", link_sigma = "log", link_nu = "logm1"))
b_sExp_schemaVR2_recall <- brm(b_sExp ~ 1,
                                 data = postDists,
                                 family = student(link = "identity", link_sigma = "log", link_nu = "logm1"))
b_IsExpMUsExp2_schemaVR2_recall <- brm(b_IsExpMUsExp ~ 1,
                                 data = postDists,
                                 family = student(link = "identity", link_sigma = "log", link_nu = "logm1"))


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
                              cores = cores2use,
                              save_all_pars = TRUE,
                              sample_prior = TRUE,
                              seed = seed) 

# /* 
# ----------------------------- Saving iamge ---------------------------
# */
save.image(datedFileNam('schemaVR3_recall', '.RData'))