# Script to run analysis of recollection data for schemaVR3 & 4 (zero prior version)
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


# /* 
# ----------------------------- Loading, preparing data and getting priors ---------------------------
# */
# Loading data
load(paste0(path2parent, "/schemaVR4/data/dataSchemaVR4_cleaned.RData"))
load(paste0(path2parent, "/schemaVR3/data/dataSchemaVR3_cleaned.RData"))

# Combine data to one data frame
dataSchemaVR3_4        <- data.frame(Experiment = c(rep('3', length(dataSchemaVR3$subNum)),
                                                    rep('4', length(dataSchemaVR4$subNum))),
                                     set        = c(as.character(dataSchemaVR3$setNum),
                                                    as.character(dataSchemaVR4$setNum)),
                                     subNum = c(as.character(dataSchemaVR3$subNum),
                                                as.character(dataSchemaVR4$subNum)),
                                     objNum = c(dataSchemaVR3$objNum,
                                                dataSchemaVR4$objNum),
                                     objLocTargetRating = c(dataSchemaVR3$objLocTargetRating,
                                                            dataSchemaVR4$objLocTargetRating),
                                     resCon = c(as.numeric(as.character(dataSchemaVR3$resCon)),
                                                as.numeric(as.character(dataSchemaVR4$resCon))))

# Creating new labels
remembered <- rep(0, dim(dataSchemaVR3_4)[1])
remembered[dataSchemaVR3_4$resCon == 1] <- 1
dataSchemaVR3_4$remembered <- remembered


# Exclude no-memory (i.e. hasn't seen object) and incorrect trials
dataSchemaVR3_4_sub <- dataSchemaVR3_4[dataSchemaVR3_4$resCon != 0, ]

# Scaling 
dataSchemaVR3_4_sub$Exp  <- dataSchemaVR3_4_sub$objLocTargetRating 
dataSchemaVR3_4_sub$sExp <- (dataSchemaVR3_4_sub$Exp - mean(dataSchemaVR3_4_sub$Exp))/(sd(dataSchemaVR3_4_sub$Exp)/0.5)

# /* 
# ----------------------------- Priors ---------------------------
# */
prior_zero  <- c(prior(student_t(7, 0, 10) , class = "Intercept"),
                 prior(student_t(7, 0, 1) , class = "b")) 
# /* 
# ----------------------------- Model ---------------------------
# */
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
save.image(datedFileNam('schemaVR3_4_recollection_zeroPrior', '.RData'))