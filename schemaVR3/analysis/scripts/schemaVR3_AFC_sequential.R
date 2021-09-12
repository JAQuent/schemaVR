# Script to run analysis of AFC accuracy data for schemaVR4 (no demean but scaled version)
# Version 1.0
# Date:  09/07/2021
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
seed <- 1383124323
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
# ----------------------------- Preparing data ---------------------------
# */
load(paste0(path2parent, "/schemaVR3/data/dataSchemaVR3_cleaned.RData"))
load(paste0(path2parent, "/schemaVR2/analysis/schemaVR2_AFC_no_demean_20210623_175512.RData"))


# Scaling 
dataSchemaVR3_AFC$Exp  <- dataSchemaVR3_AFC$objLocTargetRating 
dataSchemaVR3_AFC$sExp <- dataSchemaVR3_AFC$Exp/sd_value # sd_value is from schemaVR1 and used for all subsequent models


# /* 
# ----------------------------- Get family parameters for prior ---------------------------
# */
postDists                 <- posterior_samples(model_schemaVR2_AFC)
intercept_schemaVR2_AFC   <- brm(b_Intercept ~ 1,
                                 data = postDists,
                                 cores = cores2use,
                                 family = student(link = "identity", link_sigma = "log", link_nu = "logm1"))

# Check, beep and sleep sleepTime 10 sec
Sys.sleep(sleepTime)
beep(8)
pp_check(b_sExp_schemaVR2_AFC)


b_sExp_schemaVR2_AFC <- brm(b_sExp ~ 1,
                            data = postDists,
                            cores = cores2use,
                            family = student(link = "identity", link_sigma = "log", link_nu = "logm1"))

# Check, beep and sleep for sleepTime sec
Sys.sleep(sleepTime)
beep(8)
pp_check(b_sExp_schemaVR2_AFC)

b_IsExpMUsExp_schemaVR2_AFC <- brm(b_IsExpMUsExp ~ 1,
                                   data = postDists,
                                   cores = cores2use,
                                   family = student(link = "identity", link_sigma = "log", link_nu = "logm1"))

# Check, beep and sleep for sleepTime sec
Sys.sleep(sleepTime)
beep(8)
pp_check(b_sExp_schemaVR2_AFC)


prior_schemaVR3_AFC  <- c(set_prior(priorString_student(intercept_schemaVR2_AFC), 
                                      class = "Intercept"),
                            set_prior(priorString_student(b_sExp_schemaVR2_AFC), 
                                      class = "b", 
                                      coef = "sExp"),
                            set_prior(priorString_student(b_IsExpMUsExp_schemaVR2_AFC), 
                                      class = "b", 
                                      coef = "IsExpMUsExp"))


# /* 
# ----------------------------- Model ---------------------------
# */
model_schemaVR3_AFC <- brm(accAFC ~ sExp +  
                                I(sExp*sExp) +
                                (1 | subNum) +
                                (1 | objNum),
                           data = dataSchemaVR3_AFC,
                           prior = prior_schemaVR3_AFC,
                           family = bernoulli(),
                           chains = 8,
                           warmup = 2000,
                           iter   = 16000,
                           cores = cores2use,
                           save_all_pars = TRUE,
                           sample_prior = TRUE,
                           seed = seed,
                           control = list(adapt_delta = 0.9)) 
# Beep 
Sys.sleep(sleepTime)
summary(model_schemaVR3_AFC)
beep(8)

# /* 
# ----------------------------- Saving image ---------------------------
# */
save.image(datedFileNam('schemaVR3_AFC_sequential', '.RData'))