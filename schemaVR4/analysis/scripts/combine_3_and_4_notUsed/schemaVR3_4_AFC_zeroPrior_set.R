# Script to run analysis of AFC accuracy data for schemaVR3 & 4 with set as covariate (zero prior version)
# Version 1.0
# Date:  24/07/2021
# Author: Joern Alexander Quent
# /* 
# ----------------------------- Libraries, settings and functions ---------------------------
# */
# Ask to remove everything in the global environment
#assortedRFunctions::clear_environment()

######################################################
# Path to parent folder schemaVR
path2parent <- "D:/Alex/Laptop/Desktop/schemaVR" # This need to be changed to run this document
######################################################

# Setting WD
setwd(paste0(path2parent, "/schemaVR4/analysis"))

# Setting seed
seed <- 13831623
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
load(paste0(path2parent, "/schemaVR4/data/dataSchemaVR4_cleaned.RData"))
load(paste0(path2parent, "/schemaVR3/data/dataSchemaVR3_cleaned.RData"))

# Subsetting data
dataSchemaVR4_AFC    <- subset(dataSchemaVR4, dataSchemaVR4$resCon != '0')

dataSchemaVR3_4_AFC <- data.frame(Experiment = c(rep('3', length(dataSchemaVR3_AFC$subNum)),
                                                 rep('4', length(dataSchemaVR4_AFC$subNum))),
                                  set        = c(as.character(dataSchemaVR3_AFC$setNum),
                                                 as.character(dataSchemaVR4_AFC$setNum)),
                                  subNum = c(as.character(dataSchemaVR3_AFC$subNum),
                                             as.character(dataSchemaVR4_AFC$subNum)),
                                  objNum = c(dataSchemaVR3_AFC$objNum,
                                             dataSchemaVR4_AFC$objNum),
                                  objLocTargetRating = c(dataSchemaVR3_AFC$objLocTargetRating,
                                                         dataSchemaVR4_AFC$objLocTargetRating),
                                  accAFC = c(dataSchemaVR3_AFC$accAFC,
                                             dataSchemaVR4_AFC$accAFC))


# Scaling 
dataSchemaVR3_4_AFC$Exp  <- dataSchemaVR3_4_AFC$objLocTargetRating 
dataSchemaVR3_4_AFC$sExp <- (dataSchemaVR3_4_AFC$Exp - mean(dataSchemaVR3_4_AFC$Exp ))/(sd(dataSchemaVR3_4_AFC$Exp)/0.5)

prior_schemaVR3  <- c(prior(student_t(7, 0, 10) , class = "Intercept"),
                      prior(student_t(7, 0, 1) , class = "b")) 

# /* 
# ----------------------------- Model ---------------------------
# */
model_schemaVR3_4_AFC_set <- brm(accAFC ~ sExp*set +  
                                I(sExp*sExp)*set +
                                (1 | subNum) +
                                (1 | objNum),
                           data = dataSchemaVR3_4_AFC,
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
summary(model_schemaVR3_4_AFC_set)
beep(8)

# /* 
# ----------------------------- Saving image ---------------------------
# */
save.image(datedFileNam('schemaVR3_4_AFC_zeroPrior_set', '.RData'))