# Script to run analysis of recall accuracy data for schemaVR3 (sequential)
# Version 1.0
# Date:  25/03/2021
# Author: Joern Alexander Quent
# /* 
# ----------------------------- Libraries, settings and functions ---------------------------
# */
# Ask to remove everything in the global environment
assortedRFunctions::clear_environment()

######################################################
# Path to parent folder schemaVR
path2parent  <- "D:/Alex/Laptop/Desktop/schemaVR" # This need to be changed to run this document
path2parent1 <- path2parent # save again
######################################################

# Setting WD
setwd(paste0(path2parent, "/schemaVR3/analysis"))

# Setting seed
seed <- 13312623
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
# Loading data
load(paste0(path2parent, "/schemaVR2/analysis/schemaVR2_recall_notCentred_20210623_163732.RData"))
# Get correct path again
path2parent <- path2parent1

load(paste0(path2parent, "/schemaVR3/data/dataSchemaVR3_cleaned.RData"))

# Re creating recall DF because there was an error in the code
dataSchemaVR3_recall <- subset(dataSchemaVR3, dataSchemaVR3$recallMemory != 0 | is.na(dataSchemaVR3$recallMemory))


dataSchemaVR3_recall$Exp    <- dataSchemaVR3_recall$objLocTargetRating 
dataSchemaVR3_recall$subNum <- as.character(dataSchemaVR3_recall$subNum)
dataSchemaVR3_recall$sExp  <- (dataSchemaVR3_recall$Exp - mean(dataSchemaVR3_recall$Exp))/sd_value


# /* 
# ----------------------------- Get family parameters for prior ---------------------------
# */
postDists                 <- posterior_samples(model_schemaVR2_recall)
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

# Create prior
prior_schemaVR3_recall  <- c(set_prior(priorString_student(intercept_schemaVR2_recall), 
                                class = "Intercept"),
                             set_prior(priorString_student(b_sExp_schemaVR2_recall), 
                                class = "b", 
                                coef = "sExp"),
                             set_prior(priorString_student(b_IsExpMUsExp_schemaVR2_recall), 
                                class = "b", 
                                coef = "IsExpMUsExp"))


# /* 
# ----------------------------- Model ---------------------------
# */
model_schemaVR3_recall <- brm(accRecall ~ sExp +  
                                I(sExp*sExp) +
                                (1 | subNum) +
                                (1 | objNum),
                           data = dataSchemaVR3_recall,
                           prior = prior_schemaVR3_recall,
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
save.image(datedFileNam('schemaVR3_recall_sequential', '.RData'))
beep(1)
