#####################################################
# Warning: This script might not run with RStudio. However, it might run by just using R on its own. 
#####################################################
# Script to run analysis of AFC accuracy data for schemaVR1 and schemaVR2 to check for interaction
# Version 1.0
# Date:  16/01/2022
# Author: Joern Alexander Quent
# /* 
# ----------------------------- Libraries, settings and functions ---------------------------
# */
# Ask to remove everything in the global environment
#assortedRFunctions::clear_environment()

######################################################
# Path to parent folder schemaVR
path2parent <- "E:/Alex/Laptop/Desktop/schemaVR/" # This need to be changed to run this document
######################################################

# Setting WD
setwd(paste0(path2parent, "schemaVR2/analysis"))

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
load(paste0(path2parent, "schemaVR1/data/dataSchemaVR1_cleaned.RData"))
load(paste0(path2parent, "schemaVR2/data/dataSchemaVR2_cleaned.RData"))



combinedData_AFC <- data.frame(Experiment = c(rep('1', length(dataSchemaVR1_AFC$subNum)),
                                                 rep('2', length(dataSchemaVR2_AFC$subNum))),
                                  set        = c(rep('1', length(dataSchemaVR1_AFC$subNum)),
                                                 rep('2', length(dataSchemaVR2_AFC$subNum))),
                                  subNum = c(as.character(dataSchemaVR1_AFC$subNum),
                                             as.character(dataSchemaVR2_AFC$subNum)),
                                  objNum = c(dataSchemaVR1_AFC$objNum,
                                             dataSchemaVR2_AFC$objNum),
                                  objLocTargetRating = c(dataSchemaVR1_AFC$objLocTargetRating,
                                                         dataSchemaVR2_AFC$objLocTargetRating),
                                  accAFC = c(dataSchemaVR1_AFC$accAFC,
                                                dataSchemaVR2_AFC$accAFC))


combinedData_AFC$Exp    <- combinedData_AFC$objLocTargetRating 
combinedData_AFC$subNum <- as.character(combinedData_AFC$subNum)

# (This is not essential but does stop the Betas getting too small, since 100^2 
# gets quite big when you evaluate quadratic below).
expectancy_1_2 <- combinedData_AFC$objLocTargetRating
expectancy_1_2 <- expectancy_1_2/100

# Get mean and SD of linear and quadratic term
# Linear
AFC1_2_mean_linear <- mean(expectancy_1_2)
AFC1_2_sd_linear   <- sd(expectancy_1_2)

# Quadratic
AFC1_2_mean_quadratic <- mean(expectancy_1_2^2)
AFC1_2_sd_quadratic   <- sd(expectancy_1_2^2)


# These values are then used scale on Gelman et al. (2008) and 
# https://github.com/stan-dev/stan/wiki/Prior-Choice-Recommendations
# Mean = 0 and SD = 0.5
combinedData_AFC$Exp <- expectancy_1_2

# /* 
# ----------------------------- Prior ---------------------------
# */
prior_schemaVR2  <- c(prior(student_t(7, 0, 10) , class = "Intercept"),
                      prior(student_t(7, 0, 1) , class = "b")) 

# /* 
# ----------------------------- Model ---------------------------
# */
# Standard model
model_schemaVR1_2_AFC_m1 <- brm(accAFC ~ Experiment + I((Exp - 0.08546297)/(0.703843*2)) + I((Exp^2 -0.5008776)/(0.3671161*2)) +
                                   (1 | subNum) +
                                   (1 | objNum),
                                 data = combinedData_AFC,
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
model_schemaVR1_2_AFC_m2 <- brm(accAFC ~  Experiment + I((Exp - 0.08546297)/(0.703843*2))*Experiment + I((Exp^2 -0.5008776)/(0.3671161*2))*Experiment +
                                (1 | subNum) +
                                (1 | objNum),
                              data = combinedData_AFC,
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

## Summary
bf_interaction <- bayes_factor(model_schemaVR1_2_AFC_m1, model_schemaVR1_2_AFC_m2)
summary(model_schemaVR1_2_AFC_m2)
bf_interaction

# /* 
# ----------------------------- Saving image ---------------------------
# */
# Write file name in .txt file so it can be used across scripts
fileName   <- datedFileNam('schemaVR1_2_AFC_zeroPrior_interaction', '.RData')
write(fileName, file = paste0(path2parent, "fileNames_ofModels.txt"), append = TRUE)

# Save image
save.image(fileName)
