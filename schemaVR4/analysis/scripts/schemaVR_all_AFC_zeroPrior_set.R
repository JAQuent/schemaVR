# Script to run analysis of AFC accuracy data for schemaVR all with set as covariate (zero prior version)
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
waitTime  <- 60

# /*
# ----------------------------- Data --------------------------
# */
# Loading all .RData files
load(paste0(path2parent, "/schemaVR1/data/dataSchemaVR1_cleaned.RData"))
load(paste0(path2parent, "/schemaVR2/data/dataSchemaVR2_cleaned.RData"))
load(paste0(path2parent, "/schemaVR3/data/dataSchemaVR3_cleaned.RData"))
load(paste0(path2parent, "/schemaVR4/data/dataSchemaVR4_cleaned.RData"))

# Create AFC data for schemaVR4
dataSchemaVR4_AFC    <- subset(dataSchemaVR4, dataSchemaVR4$resCon != 0)

# Combine data to one data frame
combinedData_AFC <- data.frame(Experiment = c(rep('1', length(dataSchemaVR1_AFC$subNum)),
                                              rep('2', length(dataSchemaVR2_AFC$subNum)),
                                              rep('3', length(dataSchemaVR3_AFC$subNum)),
                                              rep('4', length(dataSchemaVR4_AFC$subNum))),
                               set        = c(rep('1', length(dataSchemaVR1_AFC$subNum)),
                                              rep('2', length(dataSchemaVR2_AFC$subNum)),
                                              as.character(dataSchemaVR3_AFC$setNum),
                                              as.character(dataSchemaVR4_AFC$setNum)),
                               subNum = c(as.character(dataSchemaVR1_AFC$subNum),
                                          as.character(dataSchemaVR2_AFC$subNum),
                                          as.character(dataSchemaVR3_AFC$subNum),
                                          as.character(dataSchemaVR4_AFC$subNum)),
                               objNum = c(dataSchemaVR1_AFC$objNum,
                                          dataSchemaVR2_AFC$objNum,
                                          dataSchemaVR3_AFC$objNum,
                                          dataSchemaVR4_AFC$objNum),
                               objLocTargetRating = c(dataSchemaVR1_AFC$objLocTargetRating,
                                                      dataSchemaVR2_AFC$objLocTargetRating,
                                                      dataSchemaVR3_AFC$objLocTargetRating,
                                                      dataSchemaVR4_AFC$objLocTargetRating),
                               accAFC = c(dataSchemaVR1_AFC$accAFC,
                                          dataSchemaVR2_AFC$accAFC,
                                          dataSchemaVR3_AFC$accAFC,
                                          dataSchemaVR4_AFC$accAFC))
# Create set names
combinedData_AFC$new_set <- as.factor(paste(combinedData_AFC$Experiment, combinedData_AFC$set, sep = '_'))

combinedData_AFC$Exp    <- combinedData_AFC$objLocTargetRating 
combinedData_AFC$subNum <- as.character(combinedData_AFC$subNum)

# Scaling based on Gelman et al. (2008) and https://github.com/stan-dev/stan/wiki/Prior-Choice-Recommendations
# Mean = 0 and SD = 0.5
combinedData_AFC$sExp <- (combinedData_AFC$Exp - mean(combinedData_AFC$Exp ))/(sd(combinedData_AFC$Exp)/0.5)
 combinedData_AFC$accAFC

# Assign to DF
df <- combinedData_AFC

prior_schemaVR_all  <- c(prior(student_t(7, 0, 10) , class = "Intercept"),
                      prior(student_t(7, 0, 1) , class = "b")) 

# /* 
# ----------------------------- Model ---------------------------
# */
# Model 1
model_schemaVR_all_AFC_set1 <- brm(accAFC ~ sExp +  
                                     I(sExp*sExp) + set +
                                     (1 | subNum) +
                                     (1 | objNum),
                                   data = combinedData_AFC,
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
summary(model_schemaVR_all_AFC_set1)
beep(8)

# Model 2
model_schemaVR_all_AFC_set2 <- brm(accAFC ~ sExp*set +  
                                I(sExp*sExp)*set +
                                (1 | subNum) +
                                (1 | objNum),
                           data = combinedData_AFC,
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
summary(model_schemaVR_all_AFC_set2)
beep(8)

# Model 3
model_schemaVR_all_AFC_no_set <- brm(accAFC ~ sExp +  
                                     I(sExp*sExp) +
                                     (1 | subNum) +
                                     (1 | objNum),
                                   data = combinedData_AFC,
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
summary(model_schemaVR_all_AFC_no_set)
beep(8)



# /* 
# ----------------------------- Saving image ---------------------------
# */
save.image(datedFileNam('schemaVR_all_AFC_zeroPrior_set', '.RData'))