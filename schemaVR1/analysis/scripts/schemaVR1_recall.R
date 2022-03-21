# Script to run analysis of recall accuracy data for schemaVR1
# Version 3.0
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
######################################################
# Path to parent folder schemaVR
path2parent <- "E:/Alex/Laptop/Desktop/schemaVR/" # This need to be changed to run this document
path2parent2 <- path2parent # Important to reset path2parent if the loaded file uses a different one
######################################################

# Setting WD
setwd(paste0(path2parent, "schemaVR1/analysis"))

# Setting seed
seed <- 724462
set.seed(seed)
seeds <- sample(1:9999, 3)

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
path2parent <- path2parent2 # Reset 

# (This is not essential but does stop the Betas getting too small, since 100^2 
# gets quite big when you evaluate quadratic below).
expectancy_1 <- dataSchemaVR1_recall$objLocTargetRating
expectancy_1 <- expectancy_1/100

# Get mean and SD of linear and quadratic term
# Linear
recall_mean_linear <- mean(expectancy_1)
recall_sd_linear   <- sd(expectancy_1)

# Quadratic
recall_mean_quadratic <- mean(expectancy_1^2)
recall_sd_quadratic   <- sd(expectancy_1^2)

# Cubic 
recall_mean_cubic <- mean(expectancy_1^3)
recall_sd_cubic   <- sd(expectancy_1^3)

# These values are then used scale on Gelman et al. (2008) and 
# https://github.com/stan-dev/stan/wiki/Prior-Choice-Recommendations
# Mean = 0 and SD = 0.5
dataSchemaVR1_recall$Exp <- expectancy_1

# /* 
# ----------------------------- Prior ---------------------------
# */
# Based on Gelman et al. (2008) and https://github.com/stan-dev/stan/wiki/Prior-Choice-Recommendations
prior_schemaVR1  <- c(prior(student_t(7, 0, 10) , class = "Intercept"),
                      prior(student_t(7, 0, 1) , class = "b")) 

# /* 
# ----------------------------- Model with random intercept ---------------------------
# */
# Using recall_mean_linear, recall_sd_linear, recall_mean_quadratic & recall_sd_quadratic
# from schemaVR1 to scale the linear and quadratic terms
# For further information see above. 
model_schemaVR1_recall <- brm(accRecall ~ I((Exp - 0.1055907)/(0.6829275*2)) + I((Exp^2 - 0.4757246)/(0.3631597*2)) + # scaled linear and scaled quad
                             (1 | subNum) +
                             (1 | objNum),
                           data = dataSchemaVR1_recall,
                           prior = prior_schemaVR1,
                           family = bernoulli(),
                           chains = 8,
                           warmup = 2000,
                           iter   = 16000,
                           cores = cores2use,
                           save_pars = save_pars(all = TRUE),
                           sample_prior = TRUE,
                           seed = seeds[1]) 

# /* 
# ----------------------------- Model with random intercept and slopes ---------------------------
# */
# Using recall_mean_linear, recall_sd_linear, recall_mean_quadratic & recall_sd_quadratic
# from schemaVR1 to scale the linear and quadratic terms
# For further information see above. 
model_schemaVR1_recall_fullRandom <- brm(accRecall ~ I((Exp - 0.1055907)/(0.6829275*2)) + I((Exp^2 - 0.4757246)/(0.3631597*2))      + # scaled linear and scaled quad
                                        (I((Exp - 0.1055907)/(0.6829275*2)) + I((Exp^2 - 0.4757246)/(0.3631597*2)) | subNum) + # scaled linear and scaled quad vary as subNum
                                        (I((Exp - 0.1055907)/(0.6829275*2)) + I((Exp^2 - 0.4757246)/(0.3631597*2)) | objNum),  # scaled linear and scaled quad vary as objNum
                                      data = dataSchemaVR1_recall,
                                      prior = prior_schemaVR1,
                                      family = bernoulli(),
                                      chains = 8,
                                      warmup = 2000,
                                      iter   = 16000,
                                      cores = cores2use,
                                      save_pars = save_pars(all = TRUE),
                                      sample_prior = TRUE,
                                      seed = seeds[2],
                                      control = list(adapt_delta = 0.98)) 

# /* 
# ----------------------------- Model without random intercept (& slope) ---------------------------
# */
# Using recall_mean_linear, recall_sd_linear, recall_mean_quadratic & recall_sd_quadratic
# from schemaVR1 to scale the linear and quadratic terms
# For further information see above. 
model_schemaVR1_recall_withoutRandom  <- brm(accRecall ~ I((Exp - 0.1055907)/(0.6829275*2)) + I((Exp^2 - 0.4757246)/(0.3631597*2)),
                                          data = dataSchemaVR1_recall,
                                          prior = prior_schemaVR1,
                                          family = bernoulli(),
                                          chains = 8,
                                          warmup = 2000,
                                          iter   = 16000,
                                          cores = cores2use,
                                          save_pars = save_pars(all = TRUE),
                                          sample_prior = TRUE,
                                          seed = seeds[3]) 

# /* 
# ----------------------------- Calculate BFs ---------------------------
# */
BF_m1_vs_m2_recall <- bayes_factor(model_schemaVR1_recall, model_schemaVR1_recall_fullRandom)
BF_m1_vs_m3_recall <- bayes_factor(model_schemaVR1_recall, model_schemaVR1_recall_withoutRandom)

# /* 
# ----------------------------- Saving image ---------------------------
# */
# Write file name in .txt file so it can be used across scripts
fileName   <- datedFileNam('schemaVR1_recall_zeroPrior', '.RData')
write(fileName, file = paste0(path2parent, "fileNames_ofModels.txt"), append = TRUE)

# Save image
save.image(fileName)