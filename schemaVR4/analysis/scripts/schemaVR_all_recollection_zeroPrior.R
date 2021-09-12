# Script to run analysis of recollection data for schemaVR all with set as covariate (zero prior version)
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
seed <- 21612323
set.seed(seed)
seeds <- sample(1:9999, 2)

# Libraries
library(assortedRFunctions)
library(brms)
library(beepr)
library(R.utils)

# General settings
cores2use <- 8
waitTime  <- 5


# /*
# ----------------------------- Data --------------------------
# */
# Loading all .RData files
load(paste0(path2parent, "/schemaVR2/data/dataSchemaVR2_cleaned.RData"))
load(paste0(path2parent, "/schemaVR3/data/dataSchemaVR3_cleaned.RData"))
load(paste0(path2parent, "/schemaVR4/data/dataSchemaVR4_cleaned.RData"))

# Combine data to one data frame
combinedData_remember <- data.frame(Experiment = c(rep('2', length(dataSchemaVR2$subNum)),
                                                   rep('3', length(dataSchemaVR3$subNum)),
                                                   rep('4', length(dataSchemaVR4$subNum))),
                                    set        = c(rep('2', length(dataSchemaVR2$subNum)),
                                                   as.character(dataSchemaVR3$setNum),
                                                   as.character(dataSchemaVR4$setNum)),
                                    subNum = c(as.character(dataSchemaVR2$subNum),
                                               as.character(dataSchemaVR3$subNum),
                                               as.character(dataSchemaVR4$subNum)),
                                    objNum = c(dataSchemaVR2$objNum,
                                               dataSchemaVR3$objNum,
                                               dataSchemaVR4$objNum),
                                    objLocTargetRating = c(dataSchemaVR2$objLocTargetRating,
                                                           dataSchemaVR3$objLocTargetRating,
                                                           dataSchemaVR4$objLocTargetRating),
                                    resCon = c(as.integer(as.character(dataSchemaVR2$resCon)),
                                               as.integer(as.character(dataSchemaVR3$resCon)),
                                               as.integer(as.character(dataSchemaVR4$resCon))))

# Create set names
combinedData_remember$new_set <- as.factor(paste(combinedData_remember$Experiment, combinedData_remember$set, sep = '_'))


# Creating new labels
remembered <- rep(0, dim(combinedData_remember)[1])
remembered[combinedData_remember$resCon == 1] <- 1
combinedData_remember$remembered <- remembered

# Exclude no-memory (i.e. hasn't seen object) 
combinedData_remember <- combinedData_remember[combinedData_remember$resCon != 0, ]


# Scaling based on Gelman et al. (2008) and https://github.com/stan-dev/stan/wiki/Prior-Choice-Recommendations
# Mean = 0 and SD = 0.5
combinedData_remember$Exp  <- combinedData_remember$objLocTargetRating 
combinedData_remember$sExp <- (combinedData_remember$Exp - mean(combinedData_remember$Exp ))/(sd(combinedData_remember$Exp)/0.5)

# /* 
# ----------------------------- Priors ---------------------------
# */
prior_zero  <- c(prior(student_t(7, 0, 10) , class = "Intercept"),
                 prior(student_t(7, 0, 1) , class = "b")) 
# /* 
# ----------------------------- Model ---------------------------
# */
# Model 3
model_schemaVR4_rem <- brm(remembered ~ sExp +  I(sExp*sExp) +
                                   (1 | objNum) +
                                   (1 | subNum),
                           data = dataSchemaVR3_4_sub,
                           prior = prior_zero,
                           family = bernoulli(),
                           chains = 8,
                           warmup = 2000,
                           iter   = 16000,
                           cores = cores2use,
                           save_all_pars = TRUE,
                           sample_prior = TRUE,
                           seed = seeds[1])

# Beep when ready 
summary(model_schemaVR4_rem)
beep(8)

# /* 
# ----------------------------- Saving image ---------------------------
# */
save.image(datedFileNam('schemaVR_all_recollection_zeroPrior', '.RData'))