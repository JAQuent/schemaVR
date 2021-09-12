# Script to run analysis of r/k data for schemaVR4 (sequential)
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
setwd(paste0(path2parent, "/schemaVR4/analysis"))

# Setting seed
seed <- 21612323
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
# ----------------------------- Loading, preparing data and getting priors ---------------------------
# */
# Loading data
load(paste0(path2parent, "/schemaVR4/data/dataSchemaVR4_cleaned.RData"))
load(paste0(path2parent, "/schemaVR3/data/dataSchemaVR3_cleaned.RData"))
load(paste0(path2parent, "/schemaVR2/analysis/schemaVR2_RK_sequential_20210325_104700.RData"))


# Combine data to one data frame
dataSchemaVR3_4        <- data.frame(Experiment = c(rep('3', length(dataSchemaVR3$subNum)),
                                                    rep('4', length(dataSchemaVR4$subNum))),
                                     set        = c(as.character(dataSchemaVR3$setNum),
                                                    as.character(dataSchemaVR4$setNum)),
                                     subNum = c(as.character(dataSchemaVR3$subNum),
                                                as.character(dataSchemaVR4$subNum)),
                                     objNum = c(dataSchemaVR3$objNum,
                                                dataSchemaVR4$objNum),
                                     objLocTargetRating = c(dataSchemaVR3$objLocTargetRating,
                                                            dataSchemaVR4$objLocTargetRating),
                                     resCon = c(as.numeric(as.character(dataSchemaVR3$resCon)),
                                                as.numeric(as.character(dataSchemaVR4$resCon))))


# Creating new labels
remembered <- rep(0, dim(dataSchemaVR3_4)[1])
remembered[dataSchemaVR3_4$resCon == 1] <- 1
dataSchemaVR3_4$remembered <- remembered

# Coding familiar judgements as redundant
familiar_red <- rep(0, dim(dataSchemaVR3_4)[1])
familiar_red[dataSchemaVR3_4$resCon == 1] <- 1
familiar_red[dataSchemaVR3_4$resCon == 2] <- 1
dataSchemaVR3_4$familiar_red <- familiar_red

# Coding familiar judgements as independent
familiar_ind <- rep(0, dim(dataSchemaVR4)[1])
familiar_ind[dataSchemaVR3_4$resCon == 1] <- NA_integer_
familiar_ind[dataSchemaVR3_4$resCon == 2] <- 1
dataSchemaVR3_4$familiar_ind <- familiar_ind

# Exclude no-memory (i.e. hasn't seen object) and incorrect trials
dataSchemaVR3_4_sub <- dataSchemaVR3_4[dataSchemaVR3_4$resCon != 0, ]

# Scaling 
dataSchemaVR3_4_sub$Exp  <- dataSchemaVR3_4_sub$objLocTargetRating 
dataSchemaVR3_4_sub$sExp <- dataSchemaVR3_4_sub$Exp/sd_value # sd_value is from schemaVR2 and used for all subsequent models

# /* 
# ----------------------------- Priors ---------------------------
# */
# Remember
postDists                      <- posterior_samples(model_schemaVR2_rem)
intercept_schemaVR2_RK_rem     <- brm(b_Intercept ~ 1,
                                      data = postDists,
                                      cores = cores2use,
                                      family = student(link = "identity", link_sigma = "log", link_nu = "logm1"))
# Beep when ready and then sleep 10 seconds to avoid crashing
beep(8)
Sys.sleep(10)

b_sExp_schemaVR2_RK_rem        <- brm(b_sExp ~ 1,
                                      data = postDists,
                                      cores = cores2use,
                                      family = student(link = "identity", link_sigma = "log", link_nu = "logm1"))

# Beep when ready and then sleep 10 seconds to avoid crashing
beep(8)
Sys.sleep(10)

b_IsExpMUsExp_schemaVR2_RK_rem <- brm(b_IsExpMUsExp ~ 1,
                                      data = postDists,
                                      cores = cores2use,
                                      family = student(link = "identity", link_sigma = "log", link_nu = "logm1"))

# Beep when ready and then sleep 10 seconds to avoid crashing
beep(8)
Sys.sleep(10)


prior_schemaVR3_4_rem  <- c(set_prior(priorString_student(intercept_schemaVR2_RK_rem), 
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

# Beep when ready and then sleep 10 seconds to avoid crashing
beep(8)
Sys.sleep(10)

b_sExp_schemaVR2_RK_fam_red        <- brm(b_sExp ~ 1,
                                          data = postDists,
                                          cores = cores2use,
                                          family = student(link = "identity", link_sigma = "log", link_nu = "logm1"))

# Beep when ready and then sleep 10 seconds to avoid crashing
beep(8)
Sys.sleep(10)

b_IsExpMUsExp_schemaVR2_RK_fam_red <- brm(b_IsExpMUsExp ~ 1,
                                          data = postDists,
                                          cores = cores2use,
                                          family = student(link = "identity", link_sigma = "log", link_nu = "logm1"))

# Beep when ready and then sleep 10 seconds to avoid crashing
beep(8)
Sys.sleep(10)

prior_schemaVR3_4_fam_red    <- c(set_prior(priorString_student(intercept_schemaVR2_RK_fam_red), 
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

# Beep when ready and then sleep 10 seconds to avoid crashing
beep(8)
Sys.sleep(10)

b_sExp_schemaVR2_RK_fam_ind        <- brm(b_sExp ~ 1,
                                          data = postDists,
                                          cores = cores2use,
                                          family = student(link = "identity", link_sigma = "log", link_nu = "logm1"))

# Beep when ready and then sleep 10 seconds to avoid crashing
beep(8)
Sys.sleep(10)

b_IsExpMUsExp_schemaVR2_RK_fam_ind <- brm(b_IsExpMUsExp ~ 1,
                                          data = postDists,
                                          cores = cores2use,
                                          family = student(link = "identity", link_sigma = "log", link_nu = "logm1"))

# Beep when ready and then sleep 10 seconds to avoid crashing
beep(8)
Sys.sleep(10)

prior_schemaVR3_4_fam_ind    <- c(set_prior(priorString_student(intercept_schemaVR2_RK_fam_ind), 
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
model_schemaVR4_rem <- brm(remembered ~ sExp +  I(sExp*sExp)+
                                   (1 | objNum) +
                                   (1 | subNum),
                           data = dataSchemaVR3_4_sub,
                           prior = prior_schemaVR3_4_rem,
                           family = bernoulli(),
                           chains = 8,
                           warmup = 2000,
                           iter   = 16000,
                           cores = cores2use,
                           save_all_pars = TRUE,
                           sample_prior = TRUE,
                           seed = seeds[1])

# Beep when ready and then sleep 10 seconds to avoid crashing
summary(model_schemaVR4_rem)
beep(8)
Sys.sleep(10)

model_schemaVR4_familiar_red  <- brm(familiar_red ~ sExp +  I(sExp*sExp)+
                                    (1 | objNum) +
                                    (1 | subNum),
                                    data = dataSchemaVR3_4_sub,
                                    prior = prior_schemaVR3_4_fam_red,
                                    family = bernoulli(),
                                    chains = 8,
                                    warmup = 2000,
                                    iter   = 16000,
                                    cores = cores2use,
                                    save_all_pars = TRUE,
                                    sample_prior = TRUE,
                                    seed = seeds[2])

# Beep when ready and then sleep 10 seconds to avoid crashing
summary(model_schemaVR4_familiar_red)
beep(8)
Sys.sleep(10)


model_schemaVR4_familiar_ind  <- brm(familiar_ind ~ sExp +  I(sExp*sExp)+
                                    (1 | objNum) +
                                    (1 | subNum),
                                    data = dataSchemaVR3_4_sub,
                                    prior = prior_schemaVR3_4_fam_ind,
                                    family = bernoulli(),
                                    chains = 8,
                                    warmup = 2000,
                                    iter   = 16000,
                                    cores = cores2use,
                                    save_all_pars = TRUE,
                                    sample_prior = TRUE,
                                    control = list(adapt_delta = 0.9),
                                    seed = seeds[3])

# Beep when ready and then sleep 10 seconds to avoid crashing
summary(model_schemaVR4_familiar_ind)
beep(8)
Sys.sleep(10)

# /* 
# ----------------------------- Saving image ---------------------------
# */
save.image(datedFileNam('schemaVR3_4_RK_sequential', '.RData'))