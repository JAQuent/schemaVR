# Script to run analysis of recollection data for schemaVR4 (sequential & exclusive)
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
setwd(paste0(path2parent, "/schemaVR4/analysis"))

# Setting seed
seed <- 41612323
set.seed(seed)
seeds <- sample(1:9999, 2)

# Libraries
library(assortedRFunctions)
library(brms)
library(beepr)
library(R.utils)

# General settings
cores2use <- 8
sleepTime <- 30

# /* 
# ----------------------------- Loading, preparing data and getting priors ---------------------------
# */
# Loading data
load(paste0(path2parent, "/schemaVR4/data/dataSchemaVR4_cleaned.RData"))
load(paste0(path2parent, "/schemaVR3/analysis/schemaVR3_RK_exclusive_sequential_20210709_161206.RData"))

# Creating new labels
remembered <- rep(0, dim(dataSchemaVR4)[1])
remembered[dataSchemaVR4$resCon == 1] <- 1
remembered[dataSchemaVR4$resCon == 2] <- NA_integer_ # Exclude familiar trials 
dataSchemaVR4$remembered <- remembered

# Exclude no-memory (i.e. hasn't seen object) and incorrect trials
dataSchemaVR4_sub <- dataSchemaVR4[dataSchemaVR4$resCon != 0, ]

# Scaling 
dataSchemaVR4_sub$Exp  <- dataSchemaVR4_sub$objLocTargetRating 
dataSchemaVR4_sub$sExp <- dataSchemaVR4_sub$Exp/sd_value # sd_value is from schemaVR3 and used for all subsequent models

# /* 
# ----------------------------- Priors ---------------------------
# */
# Remember
postDists                      <- posterior_samples(model_schemaVR3_rem_exclusive)
intercept_schemaVR3_RK_rem     <- brm(b_Intercept ~ 1,
                                      data = postDists,
                                      cores = cores2use,
                                      family = student(link = "identity", link_sigma = "log", link_nu = "logm1"))

# Beep and sleep sleepTime sec
Sys.sleep(sleepTime)
beep(8)

b_sExp_schemaVR3_RK_rem        <- brm(b_sExp ~ 1,
                                      data = postDists,
                                      cores = cores2use,
                                      family = student(link = "identity", link_sigma = "log", link_nu = "logm1"))

# Beep and sleep sleepTime sec
Sys.sleep(sleepTime)
beep(8)

b_IsExpMUsExp_schemaVR3_RK_rem <- brm(b_IsExpMUsExp ~ 1,
                                      data = postDists,
                                      cores = cores2use,
                                      family = student(link = "identity", link_sigma = "log", link_nu = "logm1"))

# Beep and sleep sleepTime sec
Sys.sleep(sleepTime)
beep(8)

prior_schemaVR4_rem  <- c(set_prior(priorString_student(intercept_schemaVR3_RK_rem), 
                                    class = "Intercept"),
                          set_prior(priorString_student(b_sExp_schemaVR3_RK_rem), 
                                    class = "b", 
                                    coef = "sExp"),
                          set_prior(priorString_student(b_IsExpMUsExp_schemaVR3_RK_rem), 
                                    class = "b", 
                                    coef = "IsExpMUsExp"))

# /* 
# ----------------------------- Model ---------------------------
# */
model_schemaVR4_rem_exclusive <- brm(remembered ~ sExp +  I(sExp*sExp)+
                                   (1 | objNum) +
                                   (1 | subNum),
                           data = dataSchemaVR4_sub,
                           prior = prior_schemaVR4_rem,
                           family = bernoulli(),
                           chains = 8,
                           warmup = 2000,
                           iter   = 16000,
                           cores = cores2use,
                           save_all_pars = TRUE,
                           sample_prior = TRUE,
                           seed = seeds[1])

# Beep and sleep sleepTime sec
Sys.sleep(sleepTime)

# Print summary
summary(model_schemaVR4_rem_exclusive)
beep(8)

# /* 
# ----------------------------- Saving image ---------------------------
# */
save.image(datedFileNam('schemaVR4_RK_exclusive_sequential', '.RData'))

# Beep when done
beep(1)
