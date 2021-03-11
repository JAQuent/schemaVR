# Script to run analysis of AFC accuracy data for schemaVR4
# Version 1.0
# Date:  09/03/2019
# Author: Joern Alexander Quent
# /* 
# ----------------------------- Libraries, settings and functions ---------------------------
# */
# Setting seed
seed <- 13831623
set.seed(seed)
seeds <- sample(1:9999, 2)

# Libraries
library(assortedRFunctions)
library(brms)
library(beepr)
library("R.utils");


# General settings
cores2use <- 4

# /* 
# ----------------------------- Preparing data ---------------------------
# */
# Loading data
load("C:/Users/aq01/Desktop/schemaVR/schemaVR4/data/dataSchemaVR4.RData")
prior_schemaVR4_AFC <- loadToEnv("C:/Users/aq01/Desktop/schemaVR/schemaVR4/priors_for_schemaVR4_20210219_135801.RData")[["prior_schemaVR4_AFC"]];

# Subsetting data
dataSchemaVR4_AFC    <- subset(dataSchemaVR4, dataSchemaVR4$resCon != '0')

# Scaling based on Gelman et al. (2008) and https://github.com/stan-dev/stan/wiki/Prior-Choice-Recommendations
# Mean = 0 and SD = 0.5
dataSchemaVR4_AFC$Exp  <- dataSchemaVR4_AFC$objLocTargetRating 
dataSchemaVR4_AFC$sExp <- (dataSchemaVR4_AFC$Exp - mean(dataSchemaVR4_AFC$Exp))/(sd(dataSchemaVR4_AFC$Exp)/0.5)


# /* 
# ----------------------------- Model ---------------------------
# */
model_schemaVR4_AFC <- brm(accAFC ~ sExp +  
                                I(sExp*sExp) +
                                (1 | subNum) +
                                (1 | objNum),
                           data = dataSchemaVR4_AFC,
                           prior = prior_schemaVR4_AFC,
                           family = bernoulli(),
                           chains = 8,
                           warmup = 2000,
                           iter   = 16000,
                           cores = cores2use,
                           save_all_pars = TRUE,
                           sample_prior = TRUE,
                           seed = seed) 
# Beep 
summary(model_schemaVR4_AFC)
beep(8)

# /* 
# ----------------------------- Saving image ---------------------------
# */
save.image(datedFileNam('schemaVR4_AFC', '.RData'))
