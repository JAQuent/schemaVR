# Script to run analysis of r/k data from schemaVR2
# Version 2.1
# Date: 19/02/2021
# Author: Joern Alexander Quent
# /* 
# ----------------------------- Libraries, settings and functions ---------------------------
# */
# Setting seed
seed <- 33846
set.seed(seed)
seeds <- sample(1:9999, 3)

# Libraries
library(assortedRFunctions)
library(brms)
library(plyr)
library(beepr)

# General settings
cores2use <- 4

# /* 
# ----------------------------- Loading and preparing data ---------------------------
# */
load("C:/Users/aq01/Desktop/schemaVR/schemaVR2/data/dataSchemaVR2_cleaned.RData")

# Creating new labels
remembered <- rep(0, dim(dataSchemaVR2)[1])
remembered[dataSchemaVR2$resCon == 1] <- 1
dataSchemaVR2$remembered <- remembered

# Coding familiar judgements as redundant
familiar_red <- rep(0, dim(dataSchemaVR2)[1])
familiar_red[dataSchemaVR2$resCon == 1] <- 1
familiar_red[dataSchemaVR2$resCon == 2] <- 1
dataSchemaVR2$familiar_red <- familiar_red

# Coding familiar judgements as independent
familiar_ind <- rep(0, dim(dataSchemaVR2)[1])
familiar_ind[dataSchemaVR2$resCon == 1] <- NA_integer_
familiar_ind[dataSchemaVR2$resCon == 2] <- 1
dataSchemaVR2$familiar_ind <- familiar_ind

# Scaling based on Gelman et al. (2008) and https://github.com/stan-dev/stan/wiki/Prior-Choice-Recommendations
# Mean = 0 and SD = 0.5
dataSchemaVR2$Exp  <- dataSchemaVR2$objLocTargetRating 
dataSchemaVR2$sExp <- (dataSchemaVR2$Exp - mean(dataSchemaVR2$Exp ))/(sd(dataSchemaVR2$Exp)/0.5)

# Exclude no-memory (i.e. hasn't seen object)
dataSchemaVR2_sub <- dataSchemaVR2[dataSchemaVR2$resCon != 0, ]

# /* 
# ----------------------------- Prior ---------------------------
# */
prior_schemaVR2  <- c(prior(student_t(7, 0, 10) , class = "Intercept"),
                      prior(student_t(7, 0, 1) , class = "b")) 

# /* 
# ----------------------------- Model ---------------------------
# */
model_schemaVR2_rem <- brm(remembered ~ sExp +  I(sExp*sExp)+
                                   (1 | objNum) +
                                   (1 | subNum),
                           data = dataSchemaVR2_sub,
                           prior = prior_schemaVR2,
                           family = bernoulli(),
                           chains = 8,
                           warmup = 2000,
                           iter   = 16000,
                           cores = cores2use,
                           save_all_pars = TRUE,
                           sample_prior = TRUE,
                           seed = seeds[1])

# Beep when ready and then sleep 10 seconds to avoid crashing
beep(8)
Sys.sleep(10)

model_schemaVR2_familiar_red  <- brm(familiar_red ~ sExp +  I(sExp*sExp)+
                                    (1 | objNum) +
                                    (1 | subNum),
                                    data = dataSchemaVR2_sub,
                                    prior = prior_schemaVR2,
                                    family = bernoulli(),
                                    chains = 8,
                                    warmup = 2000,
                                    iter   = 16000,
                                    cores = cores2use,
                                    save_all_pars = TRUE,
                                    sample_prior = TRUE,
                                    seed = seeds[2])

# Beep when ready and then sleep 10 seconds to avoid crashing
beep(8)
Sys.sleep(10)

model_schemaVR2_familiar_ind  <- brm(familiar_ind ~ sExp +  I(sExp*sExp)+
                                    (1 | objNum) +
                                    (1 | subNum),
                                    data = dataSchemaVR2_sub,
                                    prior = prior_schemaVR2,
                                    family = bernoulli(),
                                    chains = 8,
                                    warmup = 2000,
                                    iter   = 16000,
                                    cores = cores2use,
                                    save_all_pars = TRUE,
                                    sample_prior = TRUE,
                                    seed = seeds[3])

# Beep when ready and then sleep 10 seconds to avoid crashing
beep(8)
Sys.sleep(10)

# /* 
# ----------------------------- Saving image ---------------------------
# */
save.image(datedFileNam('schemaVR2_RK', '.RData'))