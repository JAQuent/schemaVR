# Script to run analysis of recall accuracy data for all experiments with prior centred around zero and cubic term
# Version 1.0
# Date:  05/02/2022
# Author: Joern Alexander Quent
# /* 
# ----------------------------- Note on this script ---------------------------
# */
# The reason why I used the rather conventional way of scaling the linear 
# and the quadratic term inside the brm() function see
# I((Exp - 0.1055907)/(0.6829275*2)) + I((Exp^2 - 0.4757246)/(0.3631597*2))
# is because this allows us to illustrate the effect and easily create plots using
# conditional_effects(). Otherwise we could have scaled the linear and the quadratic
# term outside of the function. 
# /* 
# ----------------------------- Libraries, settings and functions ---------------------------
# */
# Ask to remove everything in the global environment
#assortedRFunctions::clear_environment()

######################################################
# Path to parent folder schemaVR
path2parent <- "E:/Alex/Laptop/Desktop/schemaVR/" # This need to be changed to run this document
######################################################


# Setting seed
seed <- 214124

# Setting WD
setwd(paste0(path2parent, "/schemaVR4/analysis"))


# /*
# ----------------------------- Libraries --------------------------
# */
library(brms)
library(rslurm)
library(polspline)
library(assortedRFunctions)

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


# (This is not essential but does stop the Betas getting too small, since 100^2 
# gets quite big when you evaluate quadratic below).
expectancy_all <- combinedData_recall$objLocTargetRating
expectancy_all <- expectancy_all/100

# Get mean and SD of linear and quadratic term
# Linear
recall_mean_linear <- mean(expectancy_all)
recall_sd_linear   <- sd(expectancy_all)

# Quadratic
recall_mean_quadratic <- mean(expectancy_all^2)
recall_sd_quadratic   <- sd(expectancy_all^2)

# Cubic 
recall_mean_cubic <- mean(expectancy_all^3)
recall_sd_cubic   <- sd(expectancy_all^3)

# These values are then used scale on Gelman et al. (2008) and 
# https://github.com/stan-dev/stan/wiki/Prior-Choice-Recommendations
# Mean = 0 and SD = 0.5
combinedData_recall$Exp <- expectancy_all


# /*
# ----------------------------- Functions --------------------------
# */
# Priors for all models
priors  <- c(prior(student_t(7, 0, 10) , class = "Intercept"),
             prior(student_t(7, 0, 1) , class = "b")) 

# /* 
# ----------------------------- Cubic Model ---------------------------
# */
model_schemaVRall_recall_cubic <- brm(accRecall ~ I((Exp - 0.01100775)/(0.686581*2)) + I((Exp^2 - 0.4713124)/(0.3765475*2)) +
                                      I((Exp^3 - -0.002819718)/(0.5624869*2)) +
                                      (1 | subNum) +
                                      (1 | objNum),
                                    data = combinedData_recall,
                                    prior = priors,
                                    family = bernoulli(),
                                    chains = 8,
                                    warmup = 2000,
                                    iter   = 16000,
                                    cores = cores2use,
                                    save_pars = save_pars(all = TRUE),
                                    sample_prior = TRUE,
                                    seed = seed) 

# /* 
# ----------------------------- Saving image ---------------------------
# */
# Write file name in .txt file so it can be used across scripts
fileName   <- datedFileNam('schemaVR_all_recall_zeroPrior_cubic', '.RData')
write(fileName, file = paste0(path2parent, "fileNames_ofModels.txt"), append = TRUE)

# Save image
save.image(fileName)
