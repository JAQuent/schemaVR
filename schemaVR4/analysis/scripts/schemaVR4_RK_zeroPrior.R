# Script to run analysis of r/k data for schemaVR4 + 4 (zero prior)
# Version 1.1
# Date:  11/05/2021
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
seed <- 21612323
set.seed(seed)
seeds <- sample(1:9999, 2)

# Libraries
library(assortedRFunctions)
library(brms)
library(beepr)
library(R.utils)

# General settings
cores2use <- 4


# /* 
# ----------------------------- Loading, preparing data and getting priors ---------------------------
# */
# Loading data
load(paste0(path2parent, "/schemaVR4/data/dataSchemaVR4_cleaned.RData"))

# Creating new labels
remembered <- rep(0, dim(dataSchemaVR4)[1])
remembered[dataSchemaVR4$resCon == 1] <- 1
dataSchemaVR4$remembered <- remembered

# Coding familiar judgments as redundant
familiar_red <- rep(0, dim(dataSchemaVR4)[1])
familiar_red[dataSchemaVR4$resCon == 1] <- 1
familiar_red[dataSchemaVR4$resCon == 2] <- 1
dataSchemaVR4$familiar_red <- familiar_red

# Coding familiar judgments as independent
familiar_ind <- rep(0, dim(dataSchemaVR4)[1])
familiar_ind[dataSchemaVR4$resCon == 1] <- NA_integer_
familiar_ind[dataSchemaVR4$resCon == 2] <- 1
dataSchemaVR4$familiar_ind <- familiar_ind

# Exclude no-memory (i.e. hasn't seen object) and incorrect trials
dataSchemaVR4_sub <- dataSchemaVR4[dataSchemaVR4$resCon != 0, ]

# Scaling 
dataSchemaVR4_sub$Exp  <- dataSchemaVR4_sub$objLocTargetRating 
dataSchemaVR4_sub$sExp <- (dataSchemaVR4_sub$Exp - mean(dataSchemaVR4_sub$Exp ))/(sd(dataSchemaVR4_sub$Exp)/0.5)

# /* 
# ----------------------------- Priors ---------------------------
# */
prior_zero  <- c(prior(student_t(7, 0, 10) , class = "Intercept"),
                 prior(student_t(7, 0, 1) , class = "b")) 
# /* 
# ----------------------------- Model ---------------------------
# */
model_schemaVR4_rem <- brm(remembered ~ sExp +  I(sExp*sExp)+
                                   (1 | objNum) +
                                   (1 | subNum),
                           data = dataSchemaVR4_sub,
                           prior = prior_zero,
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
                                    data = dataSchemaVR4_sub,
                                    prior = prior_zero,
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
                                    data = dataSchemaVR4_sub,
                                    prior = prior_zero,
                                    family = bernoulli(),
                                    chains = 8,
                                    warmup = 2000,
                                    iter   = 16000,
                                    cores = cores2use,
                                    save_all_pars = TRUE,
                                    sample_prior = TRUE,
                                    seed = seeds[3])

# Beep when ready and then sleep 10 seconds to avoid crashing
summary(model_schemaVR4_familiar_ind)
beep(8)
Sys.sleep(10)

# /* 
# ----------------------------- Saving image ---------------------------
# */
save.image(datedFileNam('schemaVR4_RK_zeroPrior', '.RData'))