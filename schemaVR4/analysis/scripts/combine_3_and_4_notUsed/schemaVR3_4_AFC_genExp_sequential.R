# Script to run analysis of AFC accuracy data for schemaVR3 & 4 (no demean but scaled version)
# Version 1.0
# Date:  15/05/2021
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
cores2use <- 7


# /* 
# ----------------------------- Preparing data ---------------------------
# */
# Loading data
load(paste0(path2parent, "/schemaVR4/data/dataSchemaVR4_cleaned.RData"))
load(paste0(path2parent, "/schemaVR3/data/dataSchemaVR3_cleaned.RData"))
load(paste0(path2parent, "/schemaVR2/analysis/schemaVR2_AFC_genExp_sequential_20210514_095358.RData"))

# Subsetting data
dataSchemaVR4_AFC    <- subset(dataSchemaVR4, dataSchemaVR4$resCon != '0')

dataSchemaVR3_4_AFC <- data.frame(Experiment = c(rep('3', length(dataSchemaVR3_AFC$subNum)),
                                                 rep('4', length(dataSchemaVR4_AFC$subNum))),
                                  set        = c(as.character(dataSchemaVR3_AFC$setNum),
                                                 as.character(dataSchemaVR4_AFC$setNum)),
                                  subNum = c(as.character(dataSchemaVR3_AFC$subNum),
                                             as.character(dataSchemaVR4_AFC$subNum)),
                                  objNum = c(dataSchemaVR3_AFC$objNum,
                                             dataSchemaVR4_AFC$objNum),
                                  objLocTargetRating = c(dataSchemaVR3_AFC$objLocTargetRating,
                                                         dataSchemaVR4_AFC$objLocTargetRating),
                                  generalRatingPost = c(dataSchemaVR3_AFC$generalRatingPost,
                                                        dataSchemaVR4_AFC$generalRatingPost),
                                  accAFC = c(dataSchemaVR3_AFC$accAFC,
                                             dataSchemaVR4_AFC$accAFC))


# Scaling 
dataSchemaVR3_4_AFC$Exp  <- dataSchemaVR3_4_AFC$objLocTargetRating 
dataSchemaVR3_4_AFC$sExp <- dataSchemaVR3_4_AFC$Exp/sd_value # sd_value is from schemaVR1 and used for all subsequent models
dataSchemaVR3_4_AFC$sGeneralRatingPost <- dataSchemaVR3_4_AFC$generalRatingPost/sd_value_genExp

# /* 
# ----------------------------- Get family parameters for prior ---------------------------
# */
postDists                 <- posterior_samples(model_schemaVR2_AFC)
intercept_schemaVR2_AFC   <- brm(b_Intercept ~ 1,
                                 data = postDists,
                                 cores = cores2use,
                                 family = student(link = "identity", link_sigma = "log", link_nu = "logm1"))

# Check, beep and sleep for 10 sec
pp_check(intercept_schemaVR2_AFC)
beep(8)
Sys.sleep(10)

b_sExp_schemaVR2_AFC <- brm(b_sExp ~ 1,
                            data = postDists,
                            cores = cores2use,
                            family = student(link = "identity", link_sigma = "log", link_nu = "logm1"))

# Check, beep and sleep for 10 sec
pp_check(b_sExp_schemaVR2_AFC)
beep(8)
Sys.sleep(10)

b_IsExpMUsExp_schemaVR2_AFC <- brm(b_IsExpMUsExp ~ 1,
                                   data = postDists,
                                   cores = cores2use,
                                   family = student(link = "identity", link_sigma = "log", link_nu = "logm1"))

b_sGeneralRatingPost_schemaVR2_AFC <- brm(b_sGeneralRatingPost ~ 1,
                                          data = postDists,
                                          cores = cores2use,
                                          family = student(link = "identity", link_sigma = "log", link_nu = "logm1"))

# Check, beep and sleep for 10 sec
pp_check(b_sGeneralRatingPost_schemaVR2_AFC)
beep(8)
Sys.sleep(10)


prior_schemaVR3_4_AFC  <- c(set_prior(priorString_student(intercept_schemaVR2_AFC), 
                                class = "Intercept"),
                      set_prior(priorString_student(b_sExp_schemaVR2_AFC), 
                                class = "b", 
                                coef = "sExp"),
                      set_prior(priorString_student(b_IsExpMUsExp_schemaVR2_AFC), 
                                class = "b", 
                                coef = "IsExpMUsExp"),
                      set_prior(priorString_student(b_sGeneralRatingPost_schemaVR2_AFC), 
                                class = "b", 
                                coef = "sGeneralRatingPost"))

# /* 
# ----------------------------- Model ---------------------------
# */
model_schemaVR3_4_AFC <- brm(accAFC ~ sExp +  
                                I(sExp*sExp) + sGeneralRatingPost +
                                (1 | subNum) +
                                (1 | objNum),
                           data = dataSchemaVR3_4_AFC,
                           prior = prior_schemaVR3_4_AFC,
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
summary(model_schemaVR3_4_AFC)
beep(8)

# /* 
# ----------------------------- Saving image ---------------------------
# */
save.image(datedFileNam('schemaVR3_4_AFC_genExp_sequential', '.RData'))