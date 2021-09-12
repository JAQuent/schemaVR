# Script to run analysis of AFC highConf data for schemaVR1
# Version 1.0
# Date:  06/08/2021
# Author: Joern Alexander Quent
# /* 
# ----------------------------- Libraries, settings and functions ---------------------------
# */
assortedRFunctions::clear_environment()

######################################################
# Path to parent folder schemaVR
path2parent <- "D:/Alex/Laptop/Desktop/schemaVR" # This need to be changed to run this document
######################################################

# Setting seed
seed <- 13846
set.seed(seed)
seeds <- sample(1:9999, 2)

# Libaries
library(assortedRFunctions)
library(brms)
library(beepr)

# General settings
cores2use <- 8

# /* 
# ----------------------------- Preparing data ---------------------------
# */
# Loading all .RData files
load(paste0(path2parent, "/schemaVR1/data/dataSchemaVR1_cleaned.RData"))

# Scaling based on Gelman et al. (2008) and https://github.com/stan-dev/stan/wiki/Prior-Choice-Recommendations
# Mean = 0 and SD = 0.5
dataSchemaVR1_AFC$Exp  <- dataSchemaVR1_AFC$objLocTargetRating 
dataSchemaVR1_AFC$highConf <- ifelse(dataSchemaVR1_AFC$resCon == 3, 1, 0)
dataSchemaVR1_AFC$sExp <- (dataSchemaVR1_AFC$objLocTargetRating - mean(dataSchemaVR1_AFC$objLocTargetRating))/sd(dataSchemaVR1_AFC$objLocTargetRating)*0.5


# /* 
# ----------------------------- Model ---------------------------
# */
# Based on Gelman et al. (2008) and https://github.com/stan-dev/stan/wiki/Prior-Choice-Recommendations
prior_schemaVR1  <- c(prior(student_t(7, 0, 10) , class = "Intercept"),
                      prior(student_t(7, 0, 1) , class = "b")) 


model_schemaVR1_highConf <- brm(highConf ~ sExp +  
                                I(sExp*sExp) +
                                (1 | subNum) +
                                (1 | objNum),
                            data = dataSchemaVR1_AFC,
                            prior = prior_schemaVR1,
                            family = bernoulli(),
                            chains = 8,
                            warmup = 2000,
                            iter   = 16000,
                            cores = cores2use,
                            save_all_pars = TRUE,
                            sample_prior = TRUE,
                            seed = seed) 

# Beep and sleep
beep(8)
Sys.sleep(10)
summary(model_schemaVR1_highConf)

# /* 
# ----------------------------- Saving image ---------------------------
# */
save.image(datedFileNam('schemaVR1_highConf', '.RData'))