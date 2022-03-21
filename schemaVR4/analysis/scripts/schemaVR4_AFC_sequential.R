# Script to run analysis of AFC accuracy data for schemaVR4 (sequential)
# Version 2.0
# Date:  27/01/2022
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
# Ask to remove everything in the global environment
#assortedRFunctions::clear_environment()

######################################################
# Path to parent folder schemaVR
path2parent  <- "E:/Alex/Laptop/Desktop/schemaVR/" # This need to be changed to run this document
path2parent2 <- path2parent # Important to reset path2parent if the loaded file uses a different one
######################################################

# Setting WD
setwd(paste0(path2parent, "schemaVR4/analysis"))

# Setting seed
seed <- 124323
set.seed(seed)
seeds <- sample(1:9999, 2)

# Libraries
library(assortedRFunctions)
library(brms)
library(R.utils)

# General settings
cores2use <- 10
sleepTime <- 30

# /* 
# ----------------------------- Loading data & previous model ---------------------------
# */
# Loading data
load(paste0(path2parent, "schemaVR4/data/dataSchemaVR4_cleaned.RData"))
path2parent <- path2parent2 # Reset 

# Look for correct file name and then used it
targetPattern <- "schemaVR3_AFC_sequential"
targetFolder  <- "schemaVR3/analysis/"

# Load file with file names of models
fileName        <- paste0(path2parent, "fileNames_ofModels.txt")
conn            <- file(fileName,open="r")
model_fileNames <-readLines(conn)
close(conn)

# Select the correct model based on targetPattern
correctFile <- model_fileNames[grepl(targetPattern, model_fileNames, fixed = TRUE)]
load(paste0(path2parent, targetFolder, correctFile))
path2parent <- path2parent2 # Reset 

# /* 
# ----------------------------- Preparing data ---------------------------
# */
# (This is not essential but does stop the Betas getting too small, since 100^2 
# gets quite big when you evaluate quadratic below).
expectancy_4 <- dataSchemaVR4_AFC$objLocTargetRating
expectancy_4 <- expectancy_4/100

# These values are then used scale on Gelman et al. (2008) and 
# https://github.com/stan-dev/stan/wiki/Prior-Choice-Recommendations
# Mean = 0 and SD = 0.5
dataSchemaVR4_AFC$Exp <- expectancy_4

# /* 
# ----------------------------- Get family parameters for prior ---------------------------
# */
postDists                 <- posterior_samples(model_schemaVR3_AFC)
intercept_schemaVR3_AFC   <- brm(b_Intercept ~ 1,
                                 data = postDists,
                                 cores = cores2use,
                                 family = student(link = "identity", link_sigma = "log", link_nu = "logm1"))

# sleep sleepTime sec
Sys.sleep(sleepTime)
linear_schemaVR3_AFC <- brm(b_IExpM0.0854629D0.703843MU2 ~ 1,
                            data = postDists,
                            cores = cores2use,
                            family = student(link = "identity", link_sigma = "log", link_nu = "logm1"))

# Sleep for sleepTime sec
Sys.sleep(sleepTime)
quad_schemaVR3_AFC <- brm(b_IExpE2M0.5008776D0.3671161MU2 ~ 1,
                                   data = postDists,
                                   cores = cores2use,
                                   family = student(link = "identity", link_sigma = "log", link_nu = "logm1"))

# Sleep for sleepTime sec
Sys.sleep(sleepTime)

# Assign prior to structure
prior_schemaVR4_AFC  <- c(set_prior(priorString_student(intercept_schemaVR3_AFC), 
                                      class = "Intercept"),
                            set_prior(priorString_student(linear_schemaVR3_AFC), 
                                      class = "b", 
                                      coef = "IExpM0.0854629D0.703843MU2"),
                            set_prior(priorString_student(quad_schemaVR3_AFC), 
                                      class = "b", 
                                      coef = "IExpE2M0.5008776D0.3671161MU2"))


# /* 
# ----------------------------- Model ---------------------------
# */
# Using AFC_mean_linear, AFC_sd_linear, AFC_mean_quadratic & AFC_sd_quadratic
# from schemaVR1 to scale the linear and quadratic terms
# For further information see above. 
model_schemaVR4_AFC <- brm(accAFC ~ I((Exp - 0.0854629)/(0.703843*2)) + I((Exp^2 - 0.5008776)/(0.3671161*2)) +
                                (1 | subNum) +
                                (1 | objNum),
                           data = dataSchemaVR4_AFC,
                           prior = prior_schemaVR4_AFC,
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
summary(model_schemaVR4_AFC)


# /* 
# ----------------------------- Saving image ---------------------------
# */
# Write file name in .txt file so it can be used across scripts
fileName   <- datedFileNam('schemaVR4_AFC_sequential', '.RData')
write(fileName, file = paste0(path2parent, "fileNames_ofModels.txt"), append = TRUE)

# Save image
save.image(fileName)