# Script to model comparison of recall accuracy data for schemaVR1
# Version 1.0
# Date:  14/05/2021
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
cores2use <- 7

# /* 
# ----------------------------- Preparing data ---------------------------
# */
# Loading data
load(paste0(path2parent, "/schemaVR1/data/dataSchemaVR1_cleaned.RData"))
load(paste0(path2parent, "/schemaVR1/analysis/schemaVR1_recall_genExp_20210509_134651.RData"))

# Scaling based on Gelman et al. (2008) and https://github.com/stan-dev/stan/wiki/Prior-Choice-Recommendations
# Mean = 0 and SD = 0.5
dataSchemaVR1_recall$Exp  <- dataSchemaVR1_recall$objLocTargetRating 
dataSchemaVR1_recall$sExp <- (dataSchemaVR1_recall$Exp - mean(dataSchemaVR1_recall$Exp ))/(sd(dataSchemaVR1_recall$Exp)/0.5)
dataSchemaVR1_recall$sGeneralRatingPost <- dataSchemaVR1_recall$generalRatingPost/(sd(dataSchemaVR1_recall$generalRatingPost)/0.5)

# /* 
# ----------------------------- Model ---------------------------
# */
# Based on Gelman et al. (2008) and https://github.com/stan-dev/stan/wiki/Prior-Choice-Recommendations
prior_schemaVR1  <- c(prior(student_t(7, 0, 10) , class = "Intercept"),
                      prior(student_t(7, 0, 1) , class = "b")) 

model_schemaVR1_recall1 <- brm(accRecall ~ sExp +  
                             I(sExp*sExp) + sGeneralRatingPost +
                             (sExp + I(sExp*sExp) + sGeneralRatingPost| subNum) +
                             (sExp + I(sExp*sExp) + sGeneralRatingPost| objNum),
                           data = dataSchemaVR1_recall,
                           prior = prior_schemaVR1,
                           family = bernoulli(),
                           chains = 8,
                           warmup = 3000,
                           iter   = 19000,
                           cores = cores2use,
                           save_all_pars = TRUE,
                           sample_prior = TRUE,
                           seed = seed,
                           control = list(adapt_delta = 0.95)) 
# Beep and sleep
beep(8)
Sys.sleep(10)

model_schemaVR1_recall3 <- brm(accRecall ~ sExp +  
                                I(sExp*sExp) + sGeneralRatingPost,
                            data = dataSchemaVR1_recall,
                            prior = prior_schemaVR1,
                            family = bernoulli(),
                            chains = 8,
                            warmup = 2000,
                            iter   = 19000,
                            cores = cores2use,
                            save_all_pars = TRUE,
                            sample_prior = TRUE,
                            seed = seed) 

# Beep and sleep
beep(8)
Sys.sleep(10)

# Model with random slopes vs. model with only random intercepts
BF_randomEffect <- bayes_factor(model_schemaVR1_recall_general, model_schemaVR1_recall1)

# Model with random intercept versus model without
BF_randomEffect2 <- bayes_factor(model_schemaVR1_recall_general, model_schemaVR1_recall3)

# To save space delete everything but the BFs
all_vars <- ls()
rm(list = all_vars[!(all_vars %in% c('BF_randomEffect', 'BF_randomEffect2'))])

# /* 
# ----------------------------- Saving image ---------------------------
# */
save.image(datedFileNam('schemaVR1_recall_modelComparison', '.RData'))