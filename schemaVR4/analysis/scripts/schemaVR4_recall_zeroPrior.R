# Script to run analysis of recall accuracy data for schemaVR4 (zero prior version) with scaled Q
# Version 1.0
# Date:  05/02/2022
# Author: Joern Alexander Quent
# /* 
# ----------------------------- Note on this script ---------------------------
# */
# The reason why I used the rather conventional way of scaling the linear 
# and the quadratic term inside the brm() function see
# I((Exp - 0.1055907)/(0.6829275*2)) + I((Exp^2 - 0.4757246)/(0.3631597*2)) 
# is because this allows us to illustrate the effect and easily create plots using
# conditional_effects(). Otherwise we could have scaled the linear and the quadratic
# term outside of the function. 
# /* 
# ----------------------------- Libraries, settings and functions ---------------------------
# */
# Ask to remove everything in the global environment
#assortedRFunctions::clear_environment()

######################################################
# Path to parent folder schemaVR
path2parent <- "E:/Alex/Laptop/Desktop/schemaVR/" # This need to be changed to run this document
path2parent2 <- path2parent # Important to reset path2parent if the loaded file uses a different one
######################################################

# Setting WD
setwd(paste0(path2parent, "schemaVR4/analysis"))

# Setting seed
seed <- 11623
set.seed(seed)
seeds <- sample(1:9999, 2)

# Libraries
library(assortedRFunctions)
library(brms)
library(beepr)
library(R.utils)

# General settings
cores2use <- 10
sleepTime <- 30

# /* 
# ----------------------------- Preparing data ---------------------------
# */
# Loading data
load(paste0(path2parent, "schemaVR4/data/dataSchemaVR4_cleaned.RData"))

# (This is not essential but does stop the Betas getting too small, since 100^2 
# gets quite big when you evaluate quadratic below).
expectancy_4 <- dataSchemaVR4_recall$objLocTargetRating
expectancy_4 <- expectancy_4/100

# These values are then used scale on Gelman et al. (2008) and 
# https://github.com/stan-dev/stan/wiki/Prior-Choice-Recommendations
# Mean = 0 and SD = 0.5
dataSchemaVR4_recall$Exp <- expectancy_4

# /* 
# ----------------------------- Prior ---------------------------
# */
prior_schemaVR4  <- c(prior(student_t(7, 0, 10) , class = "Intercept"),
                      prior(student_t(7, 0, 1) , class = "b")) 

# /* 
# ----------------------------- Model ---------------------------
# */
# Using recall_mean_linear, recall_sd_linear, recall_mean_quadratic & recall_sd_quadratic
# from schemaVR1 to scale the linear and quadratic terms
# For further information see above. 
model_schemaVR4_recall <- brm(accRecall ~ I((Exp - 0.1055907)/(0.6829275*2)) + I((Exp^2 - 0.4757246)/(0.3631597*2)) +
                                (1 | subNum) +
                                (1 | objNum),
                           data = dataSchemaVR4_recall,
                           prior = prior_schemaVR4,
                           family = bernoulli(),
                           chains = 8,
                           warmup = 2000,
                           iter   = 16000,
                           cores = cores2use,
                           save_pars = save_pars(all = TRUE),
                           sample_prior = TRUE,
                           seed = seeds[1],
                           control = list(adapt_delta = 0.9)) 

# Sleep and summary 
Sys.sleep(sleepTime)
summary(model_schemaVR4_recall)

# /* 
# ----------------------------- Saving image ---------------------------
# */
# Write file name in .txt file so it can be used across scripts
fileName   <- datedFileNam('schemaVR4_recall_zeroPrior', '.RData')
write(fileName, file = paste0(path2parent, "fileNames_ofModels.txt"), append = TRUE)

# Save image
save.image(fileName)