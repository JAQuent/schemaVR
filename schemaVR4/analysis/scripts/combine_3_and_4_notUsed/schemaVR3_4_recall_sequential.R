# Script to run analysis of recall accuracy data for schemaVR4 (sequential)
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
path2parent <- "C:/Users/aq01/Desktop/schemaVR" # This need to be changed to run this document
######################################################

# Setting WD
setwd(paste0(path2parent, "/schemaVR4/analysis"))

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
cores2use <- 7

# /* 
# ----------------------------- Preparing data ---------------------------
# */
# Loading data
load(paste0(path2parent, "/schemaVR2/analysis/schemaVR2_recall_notCentred_20210516_190442.RData"))
load(paste0(path2parent, "/schemaVR3/data/dataSchemaVR3_cleaned.RData"))
load(paste0(path2parent, "/schemaVR4/data/dataSchemaVR4_cleaned.RData"))

# Re creating recall DF because there was an error in the code
dataSchemaVR3_recall <- subset(dataSchemaVR3, dataSchemaVR3$recallMemory != 0 | is.na(dataSchemaVR3$recallMemory))
dataSchemaVR4_recall <- subset(dataSchemaVR4, dataSchemaVR4$recallMemory != 0 | is.na(dataSchemaVR4$recallMemory))

# Combine data to one data frame
dataSchemaVR3_4_recall <- data.frame(Experiment = c(rep('3', length(dataSchemaVR3_recall$subNum)),
                                                    rep('4', length(dataSchemaVR4_recall$subNum))),
                                     set        = c(as.character(dataSchemaVR3_recall$setNum),
                                                    as.character(dataSchemaVR4_recall$setNum)),
                                     subNum = c(as.character(dataSchemaVR3_recall$subNum),
                                                as.character(dataSchemaVR4_recall$subNum)),
                                     objNum = c(dataSchemaVR3_recall$objNum,
                                                dataSchemaVR4_recall$objNum),
                                     objLocTargetRating = c(dataSchemaVR3_recall$objLocTargetRating,
                                                            dataSchemaVR4_recall$objLocTargetRating),
                                     accRecall = c(dataSchemaVR3_recall$accRecall,
                                                   dataSchemaVR4_recall$accRecall))

dataSchemaVR3_4_recall$Exp    <- dataSchemaVR3_4_recall$objLocTargetRating 
dataSchemaVR3_4_recall$subNum <- as.character(dataSchemaVR3_4_recall$subNum)
dataSchemaVR3_4_recall$sExp <- (dataSchemaVR3_4_recall$Exp - mean(dataSchemaVR3_4_recall$Exp))/sd_value


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
prior_schemaVR3_4_recall  <- c(set_prior(priorString_student(intercept_schemaVR2_recall), 
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
model_schemaVR3_4_recall <- brm(accRecall ~ sExp +  
                                I(sExp*sExp) +
                                (1 | subNum) +
                                (1 | objNum),
                           data = dataSchemaVR3_4_recall,
                           prior = prior_schemaVR3_4_recall,
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
summary(model_schemaVR3_4_recall)
beep(8)

# /* 
# ----------------------------- Saving image ---------------------------
# */
save.image(datedFileNam('schemaVR3_4_recall_sequential', '.RData'))
beep(1)
