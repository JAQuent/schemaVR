# Script to run analysis of recall accuracy data for schemaVR1 (with general expectancy)
# Version 1.0
# Date:  09/05/2021
# Author: Joern Alexander Quent
# /* 
# ----------------------------- Libraries, settings and functions ---------------------------
# */
# Ask to remove everything in the global environment
assortedRFunctions::clear_environment()

######################################################
# Path to parent folder schemaVR
path2parent <- "C:/Users/aq01/Desktop/schemaVR" # This need to be changed to run this document
######################################################

# Setting WD
setwd(paste0(path2parent, "/schemaVR1/analysis"))

# Setting seed
seed <- 13846
set.seed(seed)
seeds <- sample(1:9999, 2)

# Libaries
library(assortedRFunctions)
library(brms)
library(beepr)

# General settings
cores2use <- 4

# /* 
# ----------------------------- Preparing data ---------------------------
# */
# Loading data
load(paste0(path2parent, "/schemaVR1//data/dataSchemaVR1_cleaned.RData"))


# Scaling based so to have SD = 0.5. This will be used for all experiment
dataSchemaVR1_recall$Exp  <- dataSchemaVR1_recall$objLocTargetRating 

# Get scaling factor by sd()/0.5. This will be used for all subsequent experiments
sd_value <- sd(dataSchemaVR1_recall$Exp)/0.5

# Scale expectancy
dataSchemaVR1_recall$sExp <- dataSchemaVR1_recall$Exp/sd_value
dataSchemaVR1_recall$sGeneralRatingPost <- dataSchemaVR1_recall$generalRatingPost/(sd(dataSchemaVR1_recall$generalRatingPost)/0.5)


# /* 
# ----------------------------- Model ---------------------------
# */
# Based on https://jaquent.github.io/post/the-priors-that-i-use-for-logsitic-regression-now/
prior_schemaVR1  <- c(prior(student_t(7, 0, 10) , class = "Intercept"),
                      prior(student_t(7, 0, 1) , class = "b")) 


model_schemaVR1_recall_general <- brm(accRecall ~ sExp +  
                                I(sExp*sExp) + sGeneralRatingPost +
                                (1 | subNum) +
                                (1 | objNum),
                            data = dataSchemaVR1_recall,
                            prior = prior_schemaVR1,
                            family = bernoulli(),
                            chains = 8,
                            warmup = 2000,
                            iter   = 16000,
                            cores = cores2use,
                            save_all_pars = TRUE,
                            sample_prior = TRUE,
                            seed = seed,
                            control = list(adapt_delta = 0.9)) 

# /* 
# ----------------------------- Saving image ---------------------------
# */
save.image(datedFileNam('schemaVR1_recall_general_expectancy', '.RData'))