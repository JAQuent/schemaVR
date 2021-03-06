# Script to run analysis of AFC accuracy data for schemaVR2 (no demean but scaled version)
# Version 1.1
# Date:  24/03/2019
# Author: Joern Alexander Quent
# /* 
# ----------------------------- Libraries, settings and functions ---------------------------
# */
# Ask to remove everything in the global environment
assortedRFunctions::clear_environment()

######################################################
# Path to parent folder schemaVR
path2parent <- "C:/Users/aq01/Desktop/schemaVR" # This need to be changed to run this document
######################################################

# Setting WD
setwd(paste0(path2parent, "/schemaVR2/analysis"))

# Setting seed
seed <- 91248316
set.seed(seed)
seeds <- sample(1:9999, 1)

# Libraries
library(assortedRFunctions)
library(brms)
library(beepr)

# General settings
cores2use <- 4

# /* 
# ----------------------------- Preparing data ---------------------------
# */
# Loading data
load(paste0(path2parent, "/schemaVR2/data/dataSchemaVR2_cleaned.RData"))
load(paste0(path2parent, "/schemaVR1/analysis/schemaVR1_AFC_nodemean_20210324_131212.RData"))


# Scaling based so to have SD = 0.5
dataSchemaVR2_AFC$Exp  <- dataSchemaVR2_AFC$objLocTargetRating 
dataSchemaVR2_AFC$sExp <- dataSchemaVR2_AFC$Exp/sd_value # sd_value is from schemaVR1 and used for all subsequent models


# /* 
# ----------------------------- Get family parameters for prior ---------------------------
# */
postDists                 <- posterior_samples(model_schemaVR1_AFC2)
intercept_schemaVR1_AFC   <- brm(b_Intercept ~ 1,
                                 data = postDists,
                                 cores = cores2use,
                                 family = student(link = "identity", link_sigma = "log", link_nu = "logm1"))

# Check, beep and sleep for 10 sec
pp_check(intercept_schemaVR1_AFC)
beep(8)
Sys.sleep(10)


b_sExp_schemaVR1_AFC <- brm(b_sExp ~ 1,
                            data = postDists,
                            cores = cores2use,
                            family = student(link = "identity", link_sigma = "log", link_nu = "logm1"))

# Check, beep and sleep for 10 sec
pp_check(b_sExp_schemaVR1_AFC)
beep(8)
Sys.sleep(10)


b_IsExpMUsExp_schemaVR1_AFC <- brm(b_IsExpMUsExp ~ 1,
                                   data = postDists,
                                   cores = cores2use,
                                   family = student(link = "identity", link_sigma = "log", link_nu = "logm1"))

# Check, beep and sleep for 10 sec
pp_check(b_IsExpMUsExp_schemaVR1_AFC)
beep(8)
Sys.sleep(10)


prior_schemaVR2  <- c(set_prior(priorString_student(intercept_schemaVR1_AFC), 
                                class = "Intercept"),
                      set_prior(priorString_student(b_sExp_schemaVR1_AFC), 
                                class = "b", 
                                coef = "sExp"),
                      set_prior(priorString_student(b_IsExpMUsExp_schemaVR1_AFC), 
                                class = "b", 
                                coef = "IsExpMUsExp"))

# /* 
# ----------------------------- Model ---------------------------
# */
model_schemaVR2_AFC <- brm(accAFC ~ sExp +  
                                I(sExp*sExp) +
                                (1 | subNum) +
                                (1 | objNum),
                            data = dataSchemaVR2_AFC,
                            prior = prior_schemaVR2,
                            family = bernoulli(),
                            chains = 8,
                            warmup = 2000,
                            iter   = 16000,
                            cores = cores2use,
                            save_all_pars = TRUE,
                            sample_prior = TRUE,
                            seed = seed) 

# Beep
summary(model_schemaVR2_AFC)
beep(8)

# /* 
# ----------------------------- Saving image ---------------------------
# */
save.image(datedFileNam('schemaVR2_AFC_no_demean', '.RData'))