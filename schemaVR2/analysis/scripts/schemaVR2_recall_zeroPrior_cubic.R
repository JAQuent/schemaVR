#####################################################
# Warning: This script might not run with RStudio. However, it might run by just using R on its own. 
#####################################################
# Script to run analysis of recall accuracy data for schemaVR2 with prior centred around zero + a cubic term
# Version 1.0
# Date:  08/01/2022
# Author: Joern Alexander Quent
# /* 
# ----------------------------- Libraries, settings and functions ---------------------------
# */
# Ask to remove everything in the global environment
assortedRFunctions::clear_environment()

######################################################
# Path to parent folder schemaVR
path2parent <- "E:/Alex/Laptop/Desktop/schemaVR/" # This need to be changed to run this document
######################################################

# Setting WD
setwd(paste0(path2parent, "/schemaVR2/analysis"))

# Setting seed
seed <- 12846
set.seed(seed)

# Libraries
library(assortedRFunctions)
library(brms)
library(beepr)

# General settings
cores2use <- 10

# /* 
# ----------------------------- Preparing data ---------------------------
# */
# Loading data
load(paste0(path2parent, "/schemaVR2/data/dataSchemaVR2_cleaned.RData"))

# Re creating recall DF because there was an error in the code
dataSchemaVR2_recall <- subset(dataSchemaVR2, dataSchemaVR2$recallMemory != 0 | is.na(dataSchemaVR2$recallMemory))


# Scaling based on Gelman et al. (2008) and https://github.com/stan-dev/stan/wiki/Prior-Choice-Recommendations
# Mean = 0 and SD = 0.5
dataSchemaVR2_recall$Exp  <- dataSchemaVR2_recall$objLocTargetRating 
dataSchemaVR2_recall$sExp <- (dataSchemaVR2_recall$Exp - mean(dataSchemaVR2_recall$Exp))/(sd(dataSchemaVR2_recall$Exp)/0.5)


prior_schemaVR2  <- c(prior(student_t(7, 0, 10) , class = "Intercept"),
                      prior(student_t(7, 0, 1) , class = "b")) 

# /* 
# ----------------------------- Model ---------------------------
# */
# Standard model
model_schemaVR2_recall_m1 <- brm(accRecall ~ sExp +  
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
                                 seed = seed,
                                 save_pars = save_pars(all = TRUE)) 

# Sleep
Sys.sleep(10)

# Cubic model
model_schemaVR2_recall_m2 <- brm(accRecall ~ sExp +  
                                I(sExp*sExp) +
                                I(sExp*sExp*sExp) +
                                (1 | subNum) +
                                (1 | objNum),
                              data = dataSchemaVR2_recall,
                              prior = prior_schemaVR2,
                              family = bernoulli(),
                              chains = 8,
                              warmup = 2000,
                              iter   = 16000,
                              cores = cores2use,
                              seed = seed + 1,
                              save_pars = save_pars(all = TRUE)) 
# Sleep
Sys.sleep(10)

## Summary and beep
bf_cubic <- bayes_factor(model_schemaVR2_recall_m1, model_schemaVR2_recall_m2)
summary(model_schemaVR2_recall_m2)
beep(8)

# /* 
# ----------------------------- Saving image ---------------------------
# */
save.image(datedFileNam('schemaVR2_recall_zeroPrior_cubic', '.RData'))
