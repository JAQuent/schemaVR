# Script to run analysis of AFC accuracy data for schemaVR4 (no demean but scaled version)
# Version 1.0
# Date:  11/07/2021
# Author: Joern Alexander Quent
# /* 
# ----------------------------- Libraries, settings and functions ---------------------------
# */
# Ask to remove everything in the global environment
assortedRFunctions::clear_environment()

######################################################
# Path to parent folder schemaVR
path2parent <- "D:/Alex/Laptop/Desktop/schemaVR" # This need to be changed to run this document
######################################################


# Setting WD
setwd(paste0(path2parent, "/schemaVR4/analysis"))

# Setting seed
seed <- 4383
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
# ----------------------------- Preparing data ---------------------------
# */
# Loading data
load(paste0(path2parent, "/schemaVR4/data/dataSchemaVR4_cleaned.RData"))
load(paste0(path2parent, "/schemaVR3/analysis/schemaVR3_AFC_genExp_sequential_20210711_130229.RData"))

# Subsetting data
dataSchemaVR4_AFC    <- subset(dataSchemaVR4, dataSchemaVR4$resCon != '0')


# Scaling 
dataSchemaVR4_AFC$Exp  <- dataSchemaVR4_AFC$objLocTargetRating 
dataSchemaVR4_AFC$sExp <- dataSchemaVR4_AFC$Exp/sd_value # sd_value is from schemaVR1 and used for all subsequent models
dataSchemaVR4_AFC$sGeneralRatingPost <- dataSchemaVR4_AFC$generalRatingPost/sd_value_genExp

# /* 
# ----------------------------- Get family parameters for prior ---------------------------
# */
postDists                 <- posterior_samples(model_schemaVR3_AFC)
intercept_schemaVR3_AFC   <- brm(b_Intercept ~ 1,
                                 data = postDists,
                                 cores = cores2use,
                                 family = student(link = "identity", link_sigma = "log", link_nu = "logm1"))

# Check, beep and sleep for 10 sec
pp_check(intercept_schemaVR3_AFC)
beep(8)
Sys.sleep(10)

b_sExp_schemaVR3_AFC <- brm(b_sExp ~ 1,
                            data = postDists,
                            cores = cores2use,
                            family = student(link = "identity", link_sigma = "log", link_nu = "logm1"))

# Check, beep and sleep for 10 sec
pp_check(b_sExp_schemaVR3_AFC)
beep(8)
Sys.sleep(10)

b_IsExpMUsExp_schemaVR3_AFC <- brm(b_IsExpMUsExp ~ 1,
                                   data = postDists,
                                   cores = cores2use,
                                   family = student(link = "identity", link_sigma = "log", link_nu = "logm1"))

# Check, beep and sleep for 10 sec
pp_check(b_IsExpMUsExp_schemaVR3_AFC)
beep(8)
Sys.sleep(10)

b_sGeneralRatingPost_schemaVR3_AFC <- brm(b_sGeneralRatingPost ~ 1,
                                          data = postDists,
                                          cores = cores2use,
                                          family = student(link = "identity", link_sigma = "log", link_nu = "logm1"))

# Check, beep and sleep for 10 sec
pp_check(b_sGeneralRatingPost_schemaVR3_AFC)
beep(8)
Sys.sleep(10)


prior_schemaVR4_AFC  <-  c(set_prior(priorString_student(intercept_schemaVR3_AFC), 
                                class = "Intercept"),
                      set_prior(priorString_student(b_sExp_schemaVR3_AFC), 
                                class = "b", 
                                coef = "sExp"),
                      set_prior(priorString_student(b_IsExpMUsExp_schemaVR3_AFC), 
                                class = "b", 
                                coef = "IsExpMUsExp"),
                      set_prior(priorString_student(b_sGeneralRatingPost_schemaVR3_AFC), 
                                class = "b", 
                                coef = "sGeneralRatingPost"))

# /* 
# ----------------------------- Model ---------------------------
# */
model_schemaVR4_AFC <- brm(accAFC ~ sExp +  
                                I(sExp*sExp) + sGeneralRatingPost +
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
                           seed = seed,
                           control = list(adapt_delta = 0.9)) 
# Beep 
summary(model_schemaVR4_AFC)
beep(8)

# /* 
# ----------------------------- Saving image ---------------------------
# */
save.image(datedFileNam('schemaVR4_AFC_genExp_sequential', '.RData'))
