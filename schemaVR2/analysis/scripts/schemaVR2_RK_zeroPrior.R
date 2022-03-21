# Script to run analysis of r/k data from schemaVR2 centred and scaled
# Version 2.0
# Date:  05/02/2022
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
setwd(paste0(path2parent, "schemaVR2/analysis"))

# Setting seed
seed <- 33846
set.seed(seed)
seeds <- sample(1:9999, 4)

# Libraries
library(assortedRFunctions)
library(brms)
library(plyr)
library(beepr)

# General settings
cores2use <- 10
sleepTime <- 30

# /* 
# ----------------------------- Loading and preparing data ---------------------------
# */
# Loading data
load(paste0(path2parent, "schemaVR2/data/dataSchemaVR2_cleaned.RData"))

# Creating new labels
remembered <- rep(0, dim(dataSchemaVR2)[1])
remembered[dataSchemaVR2$resCon == 1] <- 1
dataSchemaVR2$remembered <- remembered

# Coding familiar judgements as redundant
familiar_red <- rep(0, dim(dataSchemaVR2)[1])
familiar_red[dataSchemaVR2$resCon == 1] <- 1
familiar_red[dataSchemaVR2$resCon == 2] <- 1
dataSchemaVR2$familiar_red <- familiar_red

# Coding familiar judgements as independent
familiar_ind <- rep(0, dim(dataSchemaVR2)[1])
familiar_ind[dataSchemaVR2$resCon == 1] <- NA_integer_
familiar_ind[dataSchemaVR2$resCon == 2] <- 1
dataSchemaVR2$familiar_ind <- familiar_ind

# Coding remember judgements as exclusive
remembered_exclu <- rep(0, dim(dataSchemaVR2)[1])
remembered_exclu[dataSchemaVR2$resCon == 1] <- 1
remembered_exclu[dataSchemaVR2$resCon == 2] <- NA_integer_ # Exclude familiar trials 
dataSchemaVR2$remembered_exclu <- remembered_exclu

# Exclude no-memory (i.e. hasn't seen object)
dataSchemaVR2_sub <- dataSchemaVR2[dataSchemaVR2$resCon != 0, ]

# (This is not essential but does stop the Betas getting too small, since 100^2 
# gets quite big when you evaluate quadratic below).
expectancy_2 <- dataSchemaVR2_sub$objLocTargetRating
expectancy_2 <- expectancy_2/100

######
# Get mean and SD of linear and quadratic term
# Linear
rem_mean_linear <- mean(expectancy_2)
rem_sd_linear   <- sd(expectancy_2)

# Quadratic
rem_mean_quadratic <- mean(expectancy_2^2)
rem_sd_quadratic   <- sd(expectancy_2^2)

# Cubic 
rem_mean_cubic <- mean(expectancy_2^3)
rem_sd_cubic  <- sd(expectancy_2^3)

######
# Do the same for familiar_ind but without the na values
familiar_ind_data <- dataSchemaVR2_sub[!is.na(dataSchemaVR2_sub$familiar_ind),]
# (This is not essential but does stop the Betas getting too small, since 100^2 

# gets quite big when you evaluate quadratic below).
expectancy_2_fam_ind <- familiar_ind_data$objLocTargetRating
expectancy_2_fam_ind <- expectancy_2_fam_ind/100

# Get mean and SD of linear and quadratic term
# Linear
familiar_ind_mean_linear <- mean(expectancy_2_fam_ind)
familiar_ind_sd_linear   <- sd(expectancy_2_fam_ind)

# Quadratic
familiar_ind_mean_quadratic <- mean(expectancy_2_fam_ind^2)
familiar_ind_sd_quadratic   <- sd(expectancy_2_fam_ind^2)

# Cubic 
familiar_ind_mean_cubic <- mean(expectancy_2_fam_ind^3)
familiar_ind_sd_cubic   <- sd(expectancy_2_fam_ind^3)

######
# Do the same for remembered_exclu but without the na values
remembered_exclu_data <- dataSchemaVR2_sub[!is.na(dataSchemaVR2_sub$remembered_exclu),]
# (This is not essential but does stop the Betas getting too small, since 100^2 

# gets quite big when you evaluate quadratic below).
expectancy_2_rem_exlcu <- remembered_exclu_data$objLocTargetRating
expectancy_2_rem_exlcu <- expectancy_2_rem_exlcu/100

# Get mean and SD of linear and quadratic term
# Linear
remembered_exclu_mean_linear <- mean(expectancy_2_rem_exlcu)
remembered_exclu_sd_linear   <- sd(expectancy_2_rem_exlcu)

# Quadratic
remembered_exclu_mean_quadratic <- mean(expectancy_2_rem_exlcu^2)
remembered_exclu_sd_quadratic   <- sd(expectancy_2_rem_exlcu^2)

# Cubic 
remembered_exclu_mean_cubic <- mean(expectancy_2_rem_exlcu^3)
remembered_exclu_sd_cubic   <- sd(expectancy_2_rem_exlcu^3)

# These values are then used scale on Gelman et al. (2008) and 
# https://github.com/stan-dev/stan/wiki/Prior-Choice-Recommendations
dataSchemaVR2_sub$Exp <- expectancy_2 # Use from the other two models because
# it should not matter from where I take the expectancy_2 values

# /* 
# ----------------------------- Prior ---------------------------
# */
prior_schemaVR2  <- c(prior(student_t(7, 0, 10) , class = "Intercept"),
                      prior(student_t(7, 0, 1) , class = "b")) 

# /* 
# ----------------------------- Model remember ---------------------------
# */
model_schemaVR2_rem <- brm(remembered ~  I((Exp - 0.08744729)/(0.6941225*2)) + I((Exp^2 - 0.4883752)/(0.3751971*2)) +
                                   (1 | objNum) +
                                   (1 | subNum),
                           data = dataSchemaVR2_sub,
                           prior = prior_schemaVR2,
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
model_schemaVR2_familiar_red  <- brm(familiar_red ~ I((Exp - 0.08744729)/(0.6941225*2)) + I((Exp^2 - 0.4883752)/(0.3751971*2)) +
                                    (1 | objNum) +
                                    (1 | subNum),
                                    data = dataSchemaVR2_sub,
                                    prior = prior_schemaVR2,
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
model_schemaVR2_familiar_ind  <- brm(familiar_ind ~ I((Exp - 0.107695)/(0.6520243*2)) + I((Exp^2 - 0.434861)/(0.3590563*2)) +
                                    (1 | objNum) +
                                    (1 | subNum),
                                    data = dataSchemaVR2_sub,
                                    prior = prior_schemaVR2,
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
model_schemaVR2_rem_exclusive <- brm(remembered_exclu ~ I((Exp - 0.07501779)/(0.7089452*2)) + I((Exp^2 - 0.5067216)/(0.3775329*2)) +
                                       (1 | objNum) +
                                       (1 | subNum),
                                     data = dataSchemaVR2_sub,
                                     prior = prior_schemaVR2,
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
fileName   <- datedFileNam('schemaVR2_RK_zeroPrior', '.RData')
write(fileName, file = paste0(path2parent, "fileNames_ofModels.txt"), append = TRUE)

# Save image
save.image(fileName)