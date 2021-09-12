# Script to run analysis of recall accuracy data for schemaVR3 (with general expectancy and sequential priors)
# Version 1.0
# Date:  10/07/2021
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
setwd(paste0(path2parent, "/schemaVR3/analysis"))

# Setting seed
seed <- 43312623
set.seed(seed)
seeds <- sample(1:9999, 2)

# Libraries
library(assortedRFunctions)
library(brms)
library(beepr)
library(R.utils)

# General settings
cores2use <- 8

# /* 
# ----------------------------- Preparing data ---------------------------
# */
load(paste0(path2parent, "/schemaVR3/data/dataSchemaVR3_cleaned.RData"))
load(paste0(path2parent, "/schemaVR2/analysis/schemaVR2_recall_genExp_sequential_20210623_171247.RData"))

# Subsetting data
dataSchemaVR3_recall <- subset(dataSchemaVR3, dataSchemaVR3$recallMemory != 0 | !is.na(dataSchemaVR3$recallMemory))


# Scaling 
dataSchemaVR3_recall$sExp  <- dataSchemaVR3_recall$objLocTargetRating/sd_value
dataSchemaVR3_recall$sGeneralRatingPost <- dataSchemaVR3_recall$generalRatingPost/sd_value_genExp

# /* 
# ----------------------------- Get family parameters for prior ---------------------------
# */
postDists                    <- posterior_samples(model_schemaVR2_recall)
intercept_schemaVR2_recall   <- brm(b_Intercept ~ 1,
                                    data = postDists,
                                    cores = cores2use,
                                    family = student(link = "identity", link_sigma = "log", link_nu = "logm1"))

# Check, beep and sleep for 10 sec
pp_check(intercept_schemaVR2_recall)
beep(8)
Sys.sleep(10)


b_sExp_schemaVR2_recall <- brm(b_sExp ~ 1,
                               data = postDists,
                               cores = cores2use,
                               family = student(link = "identity", link_sigma = "log", link_nu = "logm1"))

# Check, beep and sleep for 10 sec
pp_check(b_sExp_schemaVR2_recall)
beep(8)
Sys.sleep(10)


b_IsExpMUsExp_schemaVR2_recall <- brm(b_IsExpMUsExp ~ 1,
                                      data = postDists,
                                      cores = cores2use,
                                      family = student(link = "identity", link_sigma = "log", link_nu = "logm1"))

# Check, beep and sleep for 10 sec
pp_check(b_IsExpMUsExp_schemaVR2_recall)
beep(8)
Sys.sleep(10)


b_sGeneralRatingPost_schemaVR2_recall <- brm(b_sGeneralRatingPost ~ 1,
                                             data = postDists,
                                             cores = cores2use,
                                             family = student(link = "identity", link_sigma = "log", link_nu = "logm1"))

# Check, beep and sleep for 10 sec
pp_check(b_sGeneralRatingPost_schemaVR2_recall)
beep(8)
Sys.sleep(10)


prior_schemaVR3  <- c(set_prior(priorString_student(intercept_schemaVR2_recall), 
                                class = "Intercept"),
                      set_prior(priorString_student(b_sExp_schemaVR2_recall), 
                                class = "b", 
                                coef = "sExp"),
                      set_prior(priorString_student(b_IsExpMUsExp_schemaVR2_recall), 
                                class = "b", 
                                coef = "IsExpMUsExp"),
                      set_prior(priorString_student(b_sGeneralRatingPost_schemaVR2_recall), 
                                class = "b", 
                                coef = "sGeneralRatingPost"))


# /* 
# ----------------------------- Model ---------------------------
# */
model_schemaVR3_recall <- brm(accRecall ~ sExp +  
                                I(sExp*sExp) + sGeneralRatingPost +
                                (1 | subNum) +
                                (1 | objNum),
                           data = dataSchemaVR3_recall,
                           prior = prior_schemaVR3,
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
summary(model_schemaVR3_recall)
beep(8)

# /* 
# ----------------------------- Saving image ---------------------------
# */
save.image(datedFileNam('schemaVR3_recall_genExp_sequential', '.RData'))
