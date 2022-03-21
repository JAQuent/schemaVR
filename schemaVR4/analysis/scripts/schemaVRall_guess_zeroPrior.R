# Script to run analysis of recollection  data for all experiments with prior centred around zero and cubic term
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
set.seed(2144)


######################################################
# Path to parent folder schemaVR
path2parent <- "E:/Alex/Laptop/Desktop/schemaVR" # This need to be changed to run this document
######################################################


# Setting WD
setwd(paste0(path2parent, "/schemaVR4/analysis"))

# Libs
library(brms)
library(rslurm)
library(polspline)
library(assortedRFunctions)
library(ggplot2)

# /*
# ----------------------------- Data --------------------------
# */
# Loading all .RData files
load(paste0(path2parent, "/schemaVR1/data/dataSchemaVR1_cleaned.RData"))
load(paste0(path2parent, "/schemaVR2/data/dataSchemaVR2_cleaned.RData"))
load(paste0(path2parent, "/schemaVR3/data/dataSchemaVR3_cleaned.RData"))
load(paste0(path2parent, "/schemaVR4/data/dataSchemaVR4_cleaned.RData"))



# Combine data to one data frame
combinedData_guess <- data.frame(Experiment = c(rep('2', length(dataSchemaVR2$subNum)),
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
combinedData_guess$new_set <- as.factor(paste(combinedData_guess$Experiment, combinedData_guess$set, sep = '_'))

# Creating new labels
guess <- rep(0, dim(combinedData_guess)[1])
guess[combinedData_guess$resCon == 3] <- 1
combinedData_guess$guess <- guess


# (This is not essential but does stop the Betas getting too small, since 100^2 
# gets quite big when you evaluate quadratic below).
expectancy_all <- combinedData_guess$objLocTargetRating
expectancy_all <- expectancy_all/100

# Get mean and SD of linear and quadratic term
# Linear
guess_mean_linear <- mean(expectancy_all)
guess_sd_linear   <- sd(expectancy_all)

# Quadratic
guess_mean_quadratic <- mean(expectancy_all^2)
guess_sd_quadratic   <- sd(expectancy_all^2)

# Cubic 
guess_mean_cubic <- mean(expectancy_all^3)
guess_sd_cubic   <- sd(expectancy_all^3)

# These values are then used scale on Gelman et al. (2008) and 
# https://github.com/stan-dev/stan/wiki/Prior-Choice-Recommendations
# Mean = 0 and SD = 0.5
combinedData_guess$Exp <- expectancy_all

# /*
# ----------------------------- Functions --------------------------
# */
# Priors for all models
priors  <- c(prior(student_t(7, 0, 10) , class = "Intercept"),
             prior(student_t(7, 0, 1) , class = "b")) 

# /* 
# ----------------------------- Cubic Model ---------------------------
# */
model_schemaVRall_guess <- brm(guess ~ I((Exp - 0.002109076)/(0.6836021*2)) + I((Exp^2 - 0.4671232)/(0.3778316*2)) +
                                     (1 | subNum) +
                                     (1 | objNum),
                                   data = combinedData_guess,
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
fileName   <- datedFileNam('schemaVR_all_guess_zeroPrior', '.RData')
write(fileName, file = paste0(path2parent, "fileNames_ofModels.txt"), append = TRUE)

# Save image
save.image(fileName)