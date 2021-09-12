# Script to run analysis of r/k data for schemaVR3 (sequential)
# Version 1.0
# Date:  09/07/2021
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
setwd(paste0(path2parent, "/schemaVR3/analysis"))

# Setting seed
seed <- 11612323
set.seed(seed)
seeds <- sample(1:9999, 2)

# Libraries
library(assortedRFunctions)
library(brms)
library(beepr)
library(R.utils)

# General settings
cores2use <- 8
sleepTime <- 30

# /* 
# ----------------------------- Loading, preparing data and getting priors ---------------------------
# */
# Loading data
load(paste0(path2parent, "/schemaVR3/data/dataSchemaVR3_cleaned.RData"))
load(paste0(path2parent, "/schemaVR2/analysis/schemaVR2_RK_sequential_20210623_181032.RData"))


# Creating new labels
remembered <- rep(0, dim(dataSchemaVR3)[1])
remembered[dataSchemaVR3$resCon == 1] <- 1
dataSchemaVR3$remembered <- remembered

# Coding familiar judgements as redundant
familiar_red <- rep(0, dim(dataSchemaVR3)[1])
familiar_red[dataSchemaVR3$resCon == 1] <- 1
familiar_red[dataSchemaVR3$resCon == 2] <- 1
dataSchemaVR3$familiar_red <- familiar_red

# Coding familiar judgements as independent
familiar_ind <- rep(0, dim(dataSchemaVR3)[1])
familiar_ind[dataSchemaVR3$resCon == 1] <- NA_integer_
familiar_ind[dataSchemaVR3$resCon == 2] <- 1
dataSchemaVR3$familiar_ind <- familiar_ind

# Exclude no-memory (i.e. hasn't seen object) and incorrect trials
dataSchemaVR3_sub <- dataSchemaVR3[dataSchemaVR3$resCon != 0, ]

# Scaling 
dataSchemaVR3_sub$Exp  <- dataSchemaVR3_sub$objLocTargetRating 
dataSchemaVR3_sub$sExp <- dataSchemaVR3_sub$Exp/sd_value # sd_value is from schemaVR2 and used for all subsequent models

# /* 
# ----------------------------- Priors ---------------------------
# */
# Remember
postDists                      <- posterior_samples(model_schemaVR2_rem)
intercept_schemaVR2_RK_rem     <- brm(b_Intercept ~ 1,
                                      data = postDists,
                                      cores = cores2use,
                                      family = student(link = "identity", link_sigma = "log", link_nu = "logm1"))

# Beep and sleep sleepTime sec
Sys.sleep(sleepTime)
beep(8)


b_sExp_schemaVR2_RK_rem        <- brm(b_sExp ~ 1,
                                      data = postDists,
                                      cores = cores2use,
                                      family = student(link = "identity", link_sigma = "log", link_nu = "logm1"))

# Beep and sleep sleepTime sec
Sys.sleep(sleepTime)
beep(8)

b_IsExpMUsExp_schemaVR2_RK_rem <- brm(b_IsExpMUsExp ~ 1,
                                      data = postDists,
                                      cores = cores2use,
                                      family = student(link = "identity", link_sigma = "log", link_nu = "logm1"))

# Beep and sleep sleepTime sec
Sys.sleep(sleepTime)
beep(8)


prior_schemaVR3_rem  <- c(set_prior(priorString_student(intercept_schemaVR2_RK_rem), 
                                    class = "Intercept"),
                          set_prior(priorString_student(b_sExp_schemaVR2_RK_rem), 
                                    class = "b", 
                                    coef = "sExp"),
                          set_prior(priorString_student(b_IsExpMUsExp_schemaVR2_RK_rem), 
                                    class = "b", 
                                    coef = "IsExpMUsExp"))
# Familiar redundant
postDists                          <- posterior_samples(model_schemaVR2_familiar_red)
intercept_schemaVR2_RK_fam_red     <- brm(b_Intercept ~ 1,
                                          data = postDists,
                                          cores = cores2use,
                                          family = student(link = "identity", link_sigma = "log", link_nu = "logm1"))

# Beep and sleep sleepTime sec
Sys.sleep(sleepTime)
beep(8)

b_sExp_schemaVR2_RK_fam_red        <- brm(b_sExp ~ 1,
                                          data = postDists,
                                          cores = cores2use,
                                          family = student(link = "identity", link_sigma = "log", link_nu = "logm1"))

# Beep and sleep sleepTime sec
Sys.sleep(sleepTime)
beep(8)

b_IsExpMUsExp_schemaVR2_RK_fam_red <- brm(b_IsExpMUsExp ~ 1,
                                          data = postDists,
                                          cores = cores2use,
                                          family = student(link = "identity", link_sigma = "log", link_nu = "logm1"))
# Beep and sleep sleepTime sec
Sys.sleep(sleepTime)
beep(8)

prior_schemaVR3_fam_red    <- c(set_prior(priorString_student(intercept_schemaVR2_RK_fam_red), 
                                          class = "Intercept"),
                                set_prior(priorString_student(b_sExp_schemaVR2_RK_fam_red ), 
                                          class = "b", 
                                          coef = "sExp"),
                                set_prior(priorString_student(b_IsExpMUsExp_schemaVR2_RK_fam_red), 
                                          class = "b", 
                                          coef = "IsExpMUsExp"))

# Familiar independent
postDists                          <- posterior_samples(model_schemaVR2_familiar_ind)
intercept_schemaVR2_RK_fam_ind     <- brm(b_Intercept ~ 1,
                                          data = postDists,
                                          cores = cores2use,
                                          family = student(link = "identity", link_sigma = "log", link_nu = "logm1"))

# Beep and sleep sleepTime sec
Sys.sleep(sleepTime)
beep(8)

b_sExp_schemaVR2_RK_fam_ind        <- brm(b_sExp ~ 1,
                                          data = postDists,
                                          cores = cores2use,
                                          family = student(link = "identity", link_sigma = "log", link_nu = "logm1"))

# Beep and sleep sleepTime sec
Sys.sleep(sleepTime)
beep(8)

b_IsExpMUsExp_schemaVR2_RK_fam_ind <- brm(b_IsExpMUsExp ~ 1,
                                          data = postDists,
                                          cores = cores2use,
                                          family = student(link = "identity", link_sigma = "log", link_nu = "logm1"))

# Beep and sleep sleepTime sec
Sys.sleep(sleepTime)
beep(8)

prior_schemaVR3_fam_ind    <- c(set_prior(priorString_student(intercept_schemaVR2_RK_fam_ind), 
                                          class = "Intercept"),
                                set_prior(priorString_student(b_sExp_schemaVR2_RK_fam_ind ), 
                                          class = "b", 
                                          coef = "sExp"),
                                set_prior(priorString_student(b_IsExpMUsExp_schemaVR2_RK_fam_ind), 
                                          class = "b", 
                                          coef = "IsExpMUsExp"))

# /* 
# ----------------------------- Model ---------------------------
# */
model_schemaVR3_rem <- brm(remembered ~ sExp +  I(sExp*sExp)+
                                   (1 | objNum) +
                                   (1 | subNum),
                           data = dataSchemaVR3_sub,
                           prior = prior_schemaVR3_rem,
                           family = bernoulli(),
                           chains = 8,
                           warmup = 2000,
                           iter   = 16000,
                           cores = cores2use,
                           save_all_pars = TRUE,
                           sample_prior = TRUE,
                           seed = seeds[1])

# Beep when ready and then sleep sleepTime seconds to avoid crashing
Sys.sleep(sleepTime)
summary(model_schemaVR3_rem)
beep(8)

model_schemaVR3_familiar_red  <- brm(familiar_red ~ sExp +  I(sExp*sExp)+
                                    (1 | objNum) +
                                    (1 | subNum),
                                    data = dataSchemaVR3_sub,
                                    prior = prior_schemaVR3_fam_red,
                                    family = bernoulli(),
                                    chains = 8,
                                    warmup = 2000,
                                    iter   = 16000,
                                    cores = cores2use,
                                    save_all_pars = TRUE,
                                    sample_prior = TRUE,
                                    seed = seeds[2])

# Beep when ready and then sleep sleepTime seconds to avoid crashing
Sys.sleep(sleepTime)
summary(model_schemaVR3_familiar_red)
beep(8)


model_schemaVR3_familiar_ind  <- brm(familiar_ind ~ sExp +  I(sExp*sExp)+
                                    (1 | objNum) +
                                    (1 | subNum),
                                    data = dataSchemaVR3_sub,
                                    prior = prior_schemaVR3_fam_ind,
                                    family = bernoulli(),
                                    chains = 8,
                                    warmup = 2000,
                                    iter   = 16000,
                                    cores = cores2use,
                                    save_all_pars = TRUE,
                                    sample_prior = TRUE,
                                    control = list(adapt_delta = 0.9),
                                    seed = seeds[3])

# Beep when ready and then sleep sleepTime seconds to avoid crashing
Sys.sleep(sleepTime)
summary(model_schemaVR3_familiar_ind)
beep(8)

# /* 
# ----------------------------- Saving image ---------------------------
# */
save.image(datedFileNam('schemaVR3_RK_sequential', '.RData'))