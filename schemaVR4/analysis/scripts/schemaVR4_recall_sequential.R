# Script to run analysis of recall accuracy data for schemaVR4 (sequential)
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
path2parent  <- "D:/Alex/Laptop/Desktop/schemaVR" # This need to be changed to run this document
path2parent1 <- path2parent # save again
######################################################

# Setting WD
setwd(paste0(path2parent, "/schemaVR4/analysis"))

# Setting seed
seed <- 13312352323
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
load(paste0(path2parent, "/schemaVR3/analysis/schemaVR3_recall_sequential_20210708_170124.RData"))
# Get correct path again
path2parent <- path2parent1

load(paste0(path2parent, "/schemaVR4/data/dataSchemaVR4_cleaned.RData"))

# Re creating recall DF because there was an error in the code
dataSchemaVR4_recall <- subset(dataSchemaVR4, dataSchemaVR4$recallMemory != 0 | is.na(dataSchemaVR4$recallMemory))


dataSchemaVR4_recall$Exp    <- dataSchemaVR4_recall$objLocTargetRating 
dataSchemaVR4_recall$subNum <- as.character(dataSchemaVR4_recall$subNum)
dataSchemaVR4_recall$sExp  <- (dataSchemaVR4_recall$Exp - mean(dataSchemaVR4_recall$Exp))/sd_value


# /* 
# ----------------------------- Get family parameters for prior ---------------------------
# */
postDists                 <- posterior_samples(model_schemaVR3_recall)
intercept_schemaVR3_recall   <- brm(b_Intercept ~ 1,
                                 data = postDists,
                                 cores = cores2use,
                                 family = student(link = "identity", link_sigma = "log", link_nu = "logm1"))

# Check, beep and sleep for 10 sec
pp_check(intercept_schemaVR3_recall)
beep(8)
Sys.sleep(10)

b_sExp_schemaVR3_recall <- brm(b_sExp ~ 1,
                            data = postDists,
                            cores = cores2use,
                            family = student(link = "identity", link_sigma = "log", link_nu = "logm1"))

# Check, beep and sleep for 10 sec
pp_check(b_sExp_schemaVR3_recall)
beep(8)
Sys.sleep(10)

b_IsExpMUsExp_schemaVR3_recall <- brm(b_IsExpMUsExp ~ 1,
                                   data = postDists,
                                   cores = cores2use,
                                   family = student(link = "identity", link_sigma = "log", link_nu = "logm1"))

# Check, beep and sleep for 10 sec
pp_check(b_IsExpMUsExp_schemaVR3_recall)
beep(8)
Sys.sleep(10)

# Create prior
prior_schemaVR4_recall  <- c(set_prior(priorString_student(intercept_schemaVR3_recall), 
                                       class = "Intercept"),
                             set_prior(priorString_student(b_sExp_schemaVR3_recall), 
                                       class = "b", 
                                       coef = "sExp"),
                             set_prior(priorString_student(b_IsExpMUsExp_schemaVR3_recall), 
                                       class = "b", 
                                       coef = "IsExpMUsExp"))


# /* 
# ----------------------------- Model ---------------------------
# */
model_schemaVR4_recall <- brm(accRecall ~ sExp +  
                                I(sExp*sExp) +
                                (1 | subNum) +
                                (1 | objNum),
                           data = dataSchemaVR4_recall,
                           prior = prior_schemaVR4_recall,
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
summary(model_schemaVR4_recall)
beep(8)

# /* 
# ----------------------------- Saving image ---------------------------
# */
save.image(datedFileNam('schemaVR4_recall_sequential', '.RData'))
beep(1)
