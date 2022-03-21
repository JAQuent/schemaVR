#####################################################
# Warning: This script might not run with RStudio. However, it might run by just using R on its own. 
#####################################################
# Script to run analysis of recall accuracy data for schemaVR1 and schemaVR2 to check for interaction
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

# Re creating recall DF because there was an error in the code
dataSchemaVR2_recall <- subset(dataSchemaVR2, dataSchemaVR2$recallMemory != 0 | is.na(dataSchemaVR2$recallMemory))

combinedData_recall <- data.frame(Experiment = c(rep('1', length(dataSchemaVR1_recall$subNum)),
                                                 rep('2', length(dataSchemaVR2_recall$subNum))),
                                  set        = c(rep('1', length(dataSchemaVR1_recall$subNum)),
                                                 rep('2', length(dataSchemaVR2_recall$subNum))),
                                  subNum = c(as.character(dataSchemaVR1_recall$subNum),
                                             as.character(dataSchemaVR2_recall$subNum)),
                                  objNum = c(dataSchemaVR1_recall$objNum,
                                             dataSchemaVR2_recall$objNum),
                                  objLocTargetRating = c(dataSchemaVR1_recall$objLocTargetRating,
                                                         dataSchemaVR2_recall$objLocTargetRating),
                                  accRecall = c(dataSchemaVR1_recall$accRecall,
                                                dataSchemaVR2_recall$accRecall))


combinedData_recall$Exp    <- combinedData_recall$objLocTargetRating 
combinedData_recall$subNum <- as.character(combinedData_recall$subNum)

# (This is not essential but does stop the Betas getting too small, since 100^2 
# gets quite big when you evaluate quadratic below).
expectancy_1_2 <- combinedData_recall$objLocTargetRating
expectancy_1_2 <- expectancy_1_2/100

# Get mean and SD of linear and quadratic term
# Linear
recall1_2_mean_linear <- mean(expectancy_1_2)
recall1_2_sd_linear   <- sd(expectancy_1_2)

# Quadratic
recall1_2_mean_quadratic <- mean(expectancy_1_2^2)
recall1_2_sd_quadratic   <- sd(expectancy_1_2^2)


# These values are then used scale on Gelman et al. (2008) and 
# https://github.com/stan-dev/stan/wiki/Prior-Choice-Recommendations
# Mean = 0 and SD = 0.5
combinedData_recall$Exp <- expectancy_1_2

# /* 
# ----------------------------- Prior ---------------------------
# */
prior_schemaVR2  <- c(prior(student_t(7, 0, 10) , class = "Intercept"),
                      prior(student_t(7, 0, 1) , class = "b")) 

# /* 
# ----------------------------- Model ---------------------------
# */
# Standard model
model_schemaVR1_2_recall_m1 <- brm(accRecall ~ Experiment + I((Exp - 0.1169642)/(0.6885221*2)) + I((Exp^2 -0.487042)/(0.3709577*2)) +
                                   (1 | subNum) +
                                   (1 | objNum),
                                 data = combinedData_recall,
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
model_schemaVR1_2_recall_m2 <- brm(accRecall ~  Experiment + I((Exp - 0.1169642)/(0.6885221*2))*Experiment + I((Exp^2 -0.487042)/(0.3709577*2))*Experiment +
                                (1 | subNum) +
                                (1 | objNum),
                              data = combinedData_recall,
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
bf_interaction <- bayes_factor(model_schemaVR1_2_recall_m1, model_schemaVR1_2_recall_m2)
summary(model_schemaVR1_2_recall_m2)
bf_interaction

# /* 
# ----------------------------- Saving image ---------------------------
# */
# Write file name in .txt file so it can be used across scripts
fileName   <- datedFileNam('schemaVR1_2_recall_zeroPrior_interaction', '.RData')
write(fileName, file = paste0(path2parent, "fileNames_ofModels.txt"), append = TRUE)

# Save image
save.image(fileName)
