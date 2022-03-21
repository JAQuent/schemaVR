# Script to run analysis of AFC accuracy data for all experiments with prior centred around zero and cubic term
# Version 1.0
# Date:  13/02/2022
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

# Setting seed
set.seed(244)

######################################################
# Path to parent folder schemaVR
path2parent <- "E:/Alex/Laptop/Desktop/schemaVR" # This need to be changed to run this document
######################################################



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


# (This is not essential but does stop the Betas getting too small, since 100^2 
# gets quite big when you evaluate quadratic below).
expectancy_all <- combinedData_AFC$objLocTargetRating
expectancy_all <- expectancy_all/100

# Get mean and SD of linear and quadratic term
# Linear
AFC_mean_linear <- mean(expectancy_all)
AFC_sd_linear   <- sd(expectancy_all)

# Quadratic
AFC_mean_quadratic <- mean(expectancy_all^2)
AFC_sd_quadratic   <- sd(expectancy_all^2)

# Cubic 
AFC_mean_cubic <- mean(expectancy_all^3)
AFC_sd_cubic   <- sd(expectancy_all^3)

# These values are then used scale on Gelman et al. (2008) and 
# https://github.com/stan-dev/stan/wiki/Prior-Choice-Recommendations
# Mean = 0 and SD = 0.5
combinedData_AFC$Exp <- expectancy_all


# /*
# ----------------------------- Functions --------------------------
# */
# Priors for all models
priors  <- c(prior(student_t(7, 0, 10) , class = "Intercept"),
             prior(student_t(7, 0, 1) , class = "b")) 

# /* 
# ----------------------------- Cubic Model ---------------------------
# */
model_schemaVRall_AFC_cubic <- brm(accAFC ~ I((Exp - 0.01128509)/(0.6873529*2)) + I((Exp^2 - 0.47239)/(0.3764156*2)) +
                                      I((Exp^3 - -0.003708847)/(0.5629717*2)) +
                                      (1 | subNum) +
                                      (1 | objNum),
                                    data = combinedData_AFC,
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
fileName   <- datedFileNam('schemaVR_all_AFC_zeroPrior_cubic', '.RData')
write(fileName, file = paste0(path2parent, "fileNames_ofModels.txt"), append = TRUE)

# Save image
save.image(fileName)
