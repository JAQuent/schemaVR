# Script to run analysis of recall accuracy data for schemaVR3 (zero prior)
# Version 2.0
# Date:  16/05/2021
# Author: Joern Alexander Quent
# /* 
# ----------------------------- Libraries, settings and functions ---------------------------
# */
# Ask to remove everything in the global environment
assortedRFunctions::clear_environment()

######################################################
# Path to parent folder schemaVR
path2parent <- "D:/Alex/Laptop/Desktop/schemaVR" # This need to be changed to run this document
######################################################

# Setting WD
setwd(paste0(path2parent, "/schemaVR3/analysis"))

# Setting seed
seed <- 13312623
set.seed(seed)
seeds <- sample(1:9999, 2)

# Libraries
library(assortedRFunctions)
library(brms)
library(beepr)
library(R.utils)

# General settings
cores2use <- 8

# /* 
# ----------------------------- Preparing data ---------------------------
# */
# Loading data
load(paste0(path2parent, "/schemaVR3/data/dataSchemaVR3_cleaned.RData"))


# Scaling based on Gelman et al. (2008) and https://github.com/stan-dev/stan/wiki/Prior-Choice-Recommendations
# Mean = 0 and SD = 0.5
dataSchemaVR3_recall$Exp    <- dataSchemaVR3_recall$objLocTargetRating 
dataSchemaVR3_recall$subNum <- as.character(dataSchemaVR3_recall$subNum)
dataSchemaVR3_recall$sExp   <- (dataSchemaVR3_recall$Exp - mean(dataSchemaVR3_recall$Exp))/(sd(dataSchemaVR3_recall$Exp)/0.5)


prior_schemaVR3  <- c(prior(student_t(7, 0, 10) , class = "Intercept"),
                      prior(student_t(7, 0, 1) , class = "b")) 

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
                           seed = seed,
                           control = list(adapt_delta = 0.9)) 
# Beep 
summary(model_schemaVR3_recall)
beep(8)

# /* 
# ----------------------------- Saving image ---------------------------
# */
save.image(datedFileNam('schemaVR3_recall_zeroPrior', '.RData'))
