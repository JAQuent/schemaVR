# Script to run analysis of recollection data for schemaVR3 (zero prior & exclusive)
# Version 1.0
# Date:  08/07/2021
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
seed <- 21612323
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
# ----------------------------- Loading, preparing data and getting priors ---------------------------
# */
# Loading data
load(paste0(path2parent, "/schemaVR3/data/dataSchemaVR3_cleaned.RData"))

# Creating new labels
remembered <- rep(0, dim(dataSchemaVR3)[1])
remembered[dataSchemaVR3$resCon == 1] <- 1
remembered[dataSchemaVR3$resCon == 2] <- NA_integer_ # Exclude familiar trials 
dataSchemaVR3$remembered <- remembered

# Exclude no-memory (i.e. hasn't seen object) and incorrect trials
dataSchemaVR3_sub <- dataSchemaVR3[dataSchemaVR3$resCon != 0, ]

# Scaling 
dataSchemaVR3_sub$Exp  <- dataSchemaVR3_sub$objLocTargetRating 
dataSchemaVR3_sub$sExp <- (dataSchemaVR3_sub$Exp - mean(dataSchemaVR3_sub$Exp ))/(sd(dataSchemaVR3_sub$Exp)/0.5)
# /* 
# ----------------------------- Prior ---------------------------
# */
prior_schemaVR3_rem  <- c(prior(student_t(7, 0, 10) , class = "Intercept"),
                            prior(student_t(7, 0, 1) , class = "b")) 


#
# /* 
# ----------------------------- Model ---------------------------
# */
model_schemaVR3_rem_exclusive <- brm(remembered ~ sExp +  I(sExp*sExp)+
                                   (1 | objNum) +
                                   (1 | subNum),
                           data = dataSchemaVR3_sub,
                           prior = prior_schemaVR3_rem,
                           family = bernoulli(),
                           chains = 8,
                           warmup = 2000,
                           iter   = 16000,
                           cores = cores2use,
                           save_all_pars = TRUE,
                           sample_prior = TRUE,
                           seed = seeds[1])

# Print summary
summary(model_schemaVR3_rem_exclusive)

# /* 
# ----------------------------- Saving image ---------------------------
# */
save.image(datedFileNam('schemaVR3_RK_exclusive_zeroPrior', '.RData'))

# Beep when done
beep(1)
