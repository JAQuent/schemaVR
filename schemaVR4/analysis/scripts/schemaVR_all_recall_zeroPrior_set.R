# Script to run analysis of recall accuracy data for schemaVR all with set as covariate (zero prior version)
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
waitTime  <- 60

# /*
# ----------------------------- Data --------------------------
# */
# Loading all .RData files
load(paste0(path2parent, "/schemaVR1/data/dataSchemaVR1_cleaned.RData"))
load(paste0(path2parent, "/schemaVR2/data/dataSchemaVR2_cleaned.RData"))
load(paste0(path2parent, "/schemaVR3/data/dataSchemaVR3_cleaned.RData"))
load(paste0(path2parent, "/schemaVR4/data/dataSchemaVR4_cleaned.RData"))


# Re creating recall DF because there was an error in the code
dataSchemaVR2_recall <- subset(dataSchemaVR2, dataSchemaVR2$recallMemory != 0 | is.na(dataSchemaVR2$recallMemory))
dataSchemaVR3_recall <- subset(dataSchemaVR3, dataSchemaVR3$recallMemory != 0 | is.na(dataSchemaVR3$recallMemory))
dataSchemaVR4_recall <- subset(dataSchemaVR4, dataSchemaVR4$recallMemory != 0 | is.na(dataSchemaVR4$recallMemory))

# Combine data to one data frame
combinedData_recall <- data.frame(Experiment = c(rep('1', length(dataSchemaVR1_recall$subNum)),
                                                 rep('2', length(dataSchemaVR2_recall$subNum)),
                                                 rep('3', length(dataSchemaVR3_recall$subNum)),
                                                 rep('4', length(dataSchemaVR4_recall$subNum))),
                                  set        = c(rep('1', length(dataSchemaVR1_recall$subNum)),
                                                 rep('2', length(dataSchemaVR2_recall$subNum)),
                                                 as.character(dataSchemaVR3_recall$setNum),
                                                 as.character(dataSchemaVR4_recall$setNum)),
                                  subNum = c(as.character(dataSchemaVR1_recall$subNum),
                                             as.character(dataSchemaVR2_recall$subNum),
                                             as.character(dataSchemaVR3_recall$subNum),
                                             as.character(dataSchemaVR4_recall$subNum)),
                                  objNum = c(dataSchemaVR1_recall$objNum,
                                             dataSchemaVR2_recall$objNum,
                                             dataSchemaVR3_recall$objNum,
                                             dataSchemaVR4_recall$objNum),
                                  objLocTargetRating = c(dataSchemaVR1_recall$objLocTargetRating,
                                                         dataSchemaVR2_recall$objLocTargetRating,
                                                         dataSchemaVR3_recall$objLocTargetRating,
                                                         dataSchemaVR4_recall$objLocTargetRating),
                                  accRecall = c(dataSchemaVR1_recall$accRecall,
                                                dataSchemaVR2_recall$accRecall,
                                                dataSchemaVR3_recall$accRecall,
                                                dataSchemaVR4_recall$accRecall))

# Create set names
combinedData_recall$new_set <- as.factor(paste(combinedData_recall$Experiment, combinedData_recall$set, sep = '_'))


combinedData_recall$Exp    <- combinedData_recall$objLocTargetRating 
combinedData_recall$subNum <- as.character(combinedData_recall$subNum)

# Scaling based on Gelman et al. (2008) and https://github.com/stan-dev/stan/wiki/Prior-Choice-Recommendations
# Mean = 0 and SD = 0.5
combinedData_recall$sExp <- (combinedData_recall$Exp - mean(combinedData_recall$Exp ))/(sd(combinedData_recall$Exp)/0.5)


prior_schemaVR_all  <- c(prior(student_t(7, 0, 10) , class = "Intercept"),
                         prior(student_t(7, 0, 1) , class = "b")) 

# /* 
# ----------------------------- Model ---------------------------
# */
# Model 2
model_schemaVR_all_recall_set1 <- brm(accRecall ~ sExp +  set +
                                       I(sExp*sExp) +
                                       (1 | subNum) +
                                       (1 | objNum),
                                     data = combinedData_recall,
                                     prior = prior_schemaVR_all,
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
Sys.sleep(waitTime)
summary(model_schemaVR_all_recall_set1)
beep(8)

# Model 2
model_schemaVR_all_recall_set2 <- brm(accRecall ~ sExp*set +  
                                I(sExp*sExp)*set +
                                (1 | subNum) +
                                (1 | objNum),
                           data = combinedData_recall,
                           prior = prior_schemaVR_all,
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
Sys.sleep(waitTime)
summary(model_schemaVR_all_recall_set2)
beep(8)

# Model 3
model_schemaVR_all_recall_no_set <- brm(accRecall ~ sExp +  
                                        I(sExp*sExp) +
                                        (1 | subNum) +
                                        (1 | objNum),
                                      data = combinedData_recall,
                                      prior = prior_schemaVR_all,
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
Sys.sleep(waitTime)
summary(model_schemaVR_all_recall_no_set)
beep(8)

# /* 
# ----------------------------- Saving image ---------------------------
# */
save.image(datedFileNam('schemaVR_all_recall_zeroPrior_set', '.RData'))
