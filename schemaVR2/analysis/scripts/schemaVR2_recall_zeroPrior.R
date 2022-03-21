# Script to run analysis of recall accuracy data for schemaVR2 with prior centred around zero
# Version 2.0
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
######################################################

# Setting WD
setwd(paste0(path2parent, "/schemaVR2/analysis"))

# Setting seed
seed <- 9381316
set.seed(seed)
seeds <- sample(1:9999, 2)

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
load(paste0(path2parent, "schemaVR2/data/dataSchemaVR2_cleaned.RData"))

# (This is not essential but does stop the Betas getting too small, since 100^2 
# gets quite big when you evaluate quadratic below).
expectancy_2 <- dataSchemaVR2_recall$objLocTargetRating
expectancy_2 <- expectancy_2/100

# These values are then used scale on Gelman et al. (2008) and 
# https://github.com/stan-dev/stan/wiki/Prior-Choice-Recommendations
# Mean = 0 and SD = 0.5
dataSchemaVR2_recall$Exp <- expectancy_2

# /* 
# ----------------------------- Prior ---------------------------
# */
# Based on https://jaquent.github.io/post/the-priors-that-i-use-for-logsitic-regression-now/
prior_schemaVR2  <- c(prior(student_t(7, 0, 10) , class = "Intercept"),
                      prior(student_t(7, 0, 1) , class = "b")) 

# /* 
# ----------------------------- Model ---------------------------
# */
# Using recall_mean_linear, recall_sd_linear, recall_mean_quadratic & recall_sd_quadratic
# from schemaVR1 to scale the linear and quadratic terms
# For further information see above. 
model_schemaVR2_recall <- brm(accRecall ~ I((Exp - 0.1055907)/(0.6829275*2)) + I((Exp^2 - 0.4757246)/(0.3631597*2)) +
                                (1 | subNum) +
                                (1 | objNum),
                            data = dataSchemaVR2_recall,
                            prior = prior_schemaVR2,
                            family = bernoulli(),
                            chains = 8,
                            warmup = 2000,
                            iter   = 16000,
                            cores = cores2use,
                            save_pars = save_pars(all = TRUE),
                            sample_prior = TRUE,
                            seed = seeds[1]) 
# Sleep
Sys.sleep(30)

# /* 
# ----------------------------- Cubic Model ---------------------------
# */
model_schemaVR2_recall_cubic <- brm(accRecall ~ I((Exp - 0.1055907)/(0.6829275*2)) + I((Exp^2 - 0.4757246)/(0.3631597*2)) +
                                I((Exp^3 - 0.08330558)/(0.5464406*2)) +
                                (1 | subNum) +
                                (1 | objNum),
                              data = dataSchemaVR2_recall,
                              prior = prior_schemaVR2,
                              family = bernoulli(),
                              chains = 8,
                              warmup = 2000,
                              iter   = 16000,
                              cores = cores2use,
                              save_pars = save_pars(all = TRUE),
                              sample_prior = TRUE,
                              seed = seeds[2]) 

# /* 
# ----------------------------- BF ---------------------------
# */
BF_quad_vs_cubic <- bayes_factor(model_schemaVR2_recall, model_schemaVR2_recall_cubic)

# /* 
# ----------------------------- Saving image ---------------------------
# */
# Write file name in .txt file so it can be used across scripts
fileName   <- datedFileNam('schemaVR2_recall_zeroPrior', '.RData')
write(fileName, file = paste0(path2parent, "fileNames_ofModels.txt"), append = TRUE)

# Save image
save.image(fileName)