# Script to run analysis of recall accuracy data for schemaVR2 (with general expectancy and zero priors)
# Version 1.0
# Date:  23/06/2021
# Author: Joern Alexander Quent
# /* 
# ----------------------------- Libraries, settings and functions ---------------------------
# */
# Ask to remove everything in the global environment
assortedRFunctions::clear_environment()

######################################################
# Path to parent folder schemaVR
path2parent <- "D:/Alex/Laptop/Desktop/schemaVR/" # This need to be changed to run this document
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
cores2use <- 4

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
dataSchemaVR2_recall$sGeneralRatingPost <- dataSchemaVR2_recall$generalRatingPost/(sd(dataSchemaVR2_recall$generalRatingPost)/0.5)

prior_schemaVR2  <- c(prior(student_t(7, 0, 10) , class = "Intercept"),
                      prior(student_t(7, 0, 1) , class = "b")) 

# /* 
# ----------------------------- Model ---------------------------
# */
model_schemaVR2_recall <- brm(accRecall ~ sExp +  
                                I(sExp*sExp) + sGeneralRatingPost +
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
save.image(datedFileNam('schemaVR2_recall_genExp_zeroPrior', '.RData'))
beep(1)