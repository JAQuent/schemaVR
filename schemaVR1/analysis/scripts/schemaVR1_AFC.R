# Script to run analysis of AFC accuracy data for schemaVR1
# Date:  20/01/2022
# Author: Joern Alexander Quent
# /* 
# ----------------------------- Note on this script ---------------------------
# */
# The reason why I used the rather conventional way of scaling the linear 
# and the quadratic term inside the brm() function see
# I((Exp - 0.0854629)/(0.703843*2)) + I((Exp^2 - 0.5008776)/(0.3671161*2))
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
seed <- 13846
set.seed(seed)
seeds <- sample(1:9999, 3)

# Libraries
library(assortedRFunctions)
library(brms)
library(beepr)

# General settings
cores2use <- 10
sleepTime <- 30


# /* 
# ----------------------------- Preparing data ---------------------------
# */
# Loading data
load(paste0(path2parent, "schemaVR1/data/dataSchemaVR1_cleaned.RData"))
path2parent <- path2parent2 # Reset 

# (This is not essential but does stop the Betas getting too small, since 100^2 
# gets quite big when you evaluate quadratic below).
expectancy_1 <- dataSchemaVR1_AFC$objLocTargetRating
expectancy_1 <- expectancy_1/100

# Get mean and SD of linear and quadratic term
# Linear
AFC_mean_linear <- mean(expectancy_1)
AFC_sd_linear   <- sd(expectancy_1)

# Quadratic
AFC_mean_quadratic <- mean(expectancy_1^2)
AFC_sd_quadratic   <- sd(expectancy_1^2)

# Cubic 
AFC_mean_cubic <- mean(expectancy_1^3)
AFC_sd_cubic  <- sd(expectancy_1^3)

# These values are then used scale on Gelman et al. (2008) and 
# https://github.com/stan-dev/stan/wiki/Prior-Choice-Recommendations
dataSchemaVR1_AFC$Exp <- expectancy_1

# /* 
# ----------------------------- Prior ---------------------------
# */
# Based on Gelman et al. (2008) and https://github.com/stan-dev/stan/wiki/Prior-Choice-Recommendations
prior_schemaVR1  <- c(prior(student_t(7, 0, 10) , class = "Intercept"),
                      prior(student_t(7, 0, 1) , class = "b")) 

# /* 
# ----------------------------- Model with random intercept ---------------------------
# */
# Using AFC_mean_linear, AFC_sd_linear, AFC_mean_quadratic & AFC_sd_quadratic
# from schemaVR1 to scale the linear and quadratic terms
# For further information see above. 
model_schemaVR1_AFC <- brm(accAFC ~ I((Exp - 0.0854629)/(0.703843*2)) + I((Exp^2 - 0.5008776)/(0.3671161*2)) + # scaled linear and scaled quad
                                (1 | subNum) +
                                (1 | objNum),
                            data = dataSchemaVR1_AFC,
                            prior = prior_schemaVR1,
                            family = bernoulli(),
                            chains = 8,
                            warmup = 2000,
                            iter   = 16000,
                            cores = cores2use,
                            save_pars = save_pars(all = TRUE),
                            sample_prior = TRUE,
                            seed = seeds[1]) 
# Sleep sleepTime sec
Sys.sleep(sleepTime)

# /* 
# ----------------------------- Model with random intercept and slopes ---------------------------
# */
# Using AFC_mean_linear, AFC_sd_linear, AFC_mean_quadratic & AFC_sd_quadratic
# from schemaVR1 to scale the linear and quadratic terms
# For further information see above. 
model_schemaVR1_AFC_fullRandom <- brm(accAFC ~ I((Exp - 0.0854629)/(0.703843*2)) + I((Exp^2 - 0.5008776)/(0.3671161*2))      + # scaled linear and scaled quad
                                        (I((Exp - 0.0854629)/(0.703843*2)) +  I((Exp^2 - 0.5008776)/(0.3671161*2)) | subNum) + # scaled linear and scaled quad vary as subNum
                                        (I((Exp - 0.0854629)/(0.703843*2)) +  I((Exp^2 - 0.5008776)/(0.3671161*2)) | objNum),  # scaled linear and scaled quad vary as objNum
                                      data = dataSchemaVR1_AFC,
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
# Sleep sleepTime sec
Sys.sleep(sleepTime)

# /* 
# ----------------------------- Model without random intercept (& slope) ---------------------------
# */
# Using AFC_mean_linear, AFC_sd_linear, AFC_mean_quadratic & AFC_sd_quadratic
# from schemaVR1 to scale the linear and quadratic terms
# For further information see above. 
model_schemaVR1_AFC_withoutRandom  <- brm(accAFC ~ I((Exp - 0.0854629)/(0.703843*2)) + I((Exp^2 - 0.5008776)/(0.3671161*2)),
                                          data = dataSchemaVR1_AFC,
                                          prior = prior_schemaVR1,
                                          family = bernoulli(),
                                          chains = 8,
                                          warmup = 2000,
                                          iter   = 16000,
                                          cores = cores2use,
                                          save_pars = save_pars(all = TRUE),
                                          sample_prior = TRUE,
                                          seed = seeds[3]) 
# Sleep sleepTime sec
Sys.sleep(sleepTime)

# /* 
# ----------------------------- Calculate BFs ---------------------------
# */
BF_m1_vs_m2_AFC <- bayes_factor(model_schemaVR1_AFC, model_schemaVR1_AFC_fullRandom)

# Sleep sleepTime sec
Sys.sleep(sleepTime)

BF_m1_vs_m3_AFC <- bayes_factor(model_schemaVR1_AFC, model_schemaVR1_AFC_withoutRandom)

# Sleep sleepTime sec
Sys.sleep(sleepTime)

# /* 
# ----------------------------- Saving image ---------------------------
# */
# Write file name in .txt file so it can be used across scripts
fileName   <- datedFileNam('schemaVR1_AFC_zeroPrior', '.RData')
write(fileName, file = paste0(path2parent, "fileNames_ofModels.txt"), append = TRUE)

# Save image
save.image(fileName)