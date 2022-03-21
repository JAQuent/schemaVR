# Script to run analysis of r/k data for schemaVR3 (zero prior)
# Version 1.0
# Date:  08/07/2021
# Author: Joern Alexander Quent
# /* 
# ----------------------------- Note on this script ---------------------------
# */
# The reason why I used the rather conventional way of scaling the linear 
# and the quadratic term inside the brm() function looks similar to this
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
setwd(paste0(path2parent, "schemaVR3/analysis"))

# Setting seed
seed <- 112323 + 1
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
# ----------------------------- Loading data ---------------------------
# */
# Loading data
load(paste0(path2parent, "schemaVR3/data/dataSchemaVR3_cleaned.RData"))
path2parent <- path2parent2 # Reset 

# /* 
# ----------------------------- Preparing data ---------------------------
# */
# Creating new labels
remembered <- rep(0, dim(dataSchemaVR3)[1])
remembered[dataSchemaVR3$resCon == 1] <- 1
dataSchemaVR3$remembered <- remembered

# Coding familiar judgements as redundant
familiar_red <- rep(0, dim(dataSchemaVR3)[1])
familiar_red[dataSchemaVR3$resCon == 1] <- 1
familiar_red[dataSchemaVR3$resCon == 2] <- 1
dataSchemaVR3$familiar_red <- familiar_red

# Coding familiar judgements as independent
familiar_ind <- rep(0, dim(dataSchemaVR3)[1])
familiar_ind[dataSchemaVR3$resCon == 1] <- NA_integer_
familiar_ind[dataSchemaVR3$resCon == 2] <- 1
dataSchemaVR3$familiar_ind <- familiar_ind

# Coding remember judgements as exclusive
remembered_exclu <- rep(0, dim(dataSchemaVR3)[1])
remembered_exclu[dataSchemaVR3$resCon == 1] <- 1
remembered_exclu[dataSchemaVR3$resCon == 2] <- NA_integer_ # Exclude familiar trials 
dataSchemaVR3$remembered_exclu <- remembered_exclu

# Exclude no-memory (i.e. hasn't seen object)
dataSchemaVR3_sub <- dataSchemaVR3[dataSchemaVR3$resCon != 0, ]

# (This is not essential but does stop the Betas getting too small, since 100^2 
# gets quite big when you evaluate quadratic below).
expectancy_3 <- dataSchemaVR3_sub$objLocTargetRating
expectancy_3 <- expectancy_3/100

# These values are then used scale on Gelman et al. (2008) and 
# https://github.com/stan-dev/stan/wiki/Prior-Choice-Recommendations
dataSchemaVR3_sub$Exp <- expectancy_3 # Use from the other two models because
# it should not matter from where I take the expectancy_3 values

# /* 
# ----------------------------- Priors ---------------------------
# */
prior_zero  <- c(prior(student_t(7, 0, 10) , class = "Intercept"),
                 prior(student_t(7, 0, 1) , class = "b")) 

# /* 
# ----------------------------- Model remember ---------------------------
# */
model_schemaVR3_rem <- brm(remembered ~  I((Exp - 0.08744729)/(0.6941225*2)) + I((Exp^2 - 0.4883752)/(0.3751971*2)) +
                             (1 | objNum) +
                             (1 | subNum),
                           data = dataSchemaVR3_sub,
                           prior = prior_zero,
                           family = bernoulli(),
                           chains = 8,
                           warmup = 2000,
                           iter   = 16000,
                           cores = cores2use,
                           save_pars = save_pars(all = TRUE),
                           sample_prior = TRUE,
                           seed = seeds[1])

# Sleep 10 seconds to avoid crashing
Sys.sleep(sleepTime)

# /* 
# ----------------------------- Model familiar redundant ---------------------------
# */
model_schemaVR3_familiar_red  <- brm(familiar_red ~ I((Exp - 0.08744729)/(0.6941225*2)) + I((Exp^2 - 0.4883752)/(0.3751971*2)) +
                                       (1 | objNum) +
                                       (1 | subNum),
                                     data = dataSchemaVR3_sub,
                                     prior = prior_zero,
                                     family = bernoulli(),
                                     chains = 8,
                                     warmup = 2000,
                                     iter   = 16000,
                                     cores = cores2use,
                                     save_pars = save_pars(all = TRUE),
                                     sample_prior = TRUE,
                                     seed = seeds[2])

# Sleep 10 seconds to avoid crashing
Sys.sleep(sleepTime)

# /* 
# ----------------------------- Model familiar independent ---------------------------
# */
model_schemaVR3_familiar_ind  <- brm(familiar_ind ~ I((Exp - 0.107695)/(0.6520243*2)) + I((Exp^2 - 0.434861)/(0.3590563*2)) +
                                       (1 | objNum) +
                                       (1 | subNum),
                                     data = dataSchemaVR3_sub,
                                     prior = prior_zero,
                                     family = bernoulli(),
                                     chains = 8,
                                     warmup = 2000,
                                     iter   = 16000,
                                     cores = cores2use,
                                     save_pars = save_pars(all = TRUE),
                                     sample_prior = TRUE,
                                     seed = seeds[3])

# Sleep 10 seconds to avoid crashing
Sys.sleep(sleepTime)

# /* 
# ----------------------------- Model remember exclusive ---------------------------
# */
model_schemaVR3_rem_exclusive <- brm(remembered_exclu ~ I((Exp - 0.07501779)/(0.7089452*2)) + I((Exp^2 - 0.5067216)/(0.3775329*2)) +
                                       (1 | objNum) +
                                       (1 | subNum),
                                     data = dataSchemaVR3_sub,
                                     prior = prior_zero,
                                     family = bernoulli(),
                                     chains = 8,
                                     warmup = 2000,
                                     iter   = 16000,
                                     cores = cores2use,
                                     save_pars = save_pars(all = TRUE),
                                     sample_prior = TRUE,
                                     seed = seeds[4])

# /* 
# ----------------------------- Saving image ---------------------------
# */
# Write file name in .txt file so it can be used across scripts
fileName   <- datedFileNam('schemaVR3_RK_zeroPrior', '.RData')
write(fileName, file = paste0(path2parent, "fileNames_ofModels.txt"), append = TRUE)

# Save image
save.image(fileName)