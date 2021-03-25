# Script to run analysis of r/k data for schemaVR4
# Version 1.0
# Date:  11/03/2019
# Author: Joern Alexander Quent
# /* 
# ----------------------------- Libraries, settings and functions ---------------------------
# */
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

# Setting wd
setwd("C:/Users/aq01/Desktop/schemaVR/schemaVR4/analysis")

# /* 
# ----------------------------- Loading, preparing data and getting priors ---------------------------
# */
# Loading data
load("C:/Users/aq01/Desktop/schemaVR/schemaVR4/data/dataSchemaVR4_cleaned.RData")

# To save memory only load prior that is needed
prior_schemaVR4_rem <- loadToEnv("C:/Users/aq01/Desktop/schemaVR/schemaVR4/priors_for_schemaVR4_20210219_135801.RData")[["prior_schemaVR4_rem"]];
prior_schemaVR4_familiar_ind <- loadToEnv("C:/Users/aq01/Desktop/schemaVR/schemaVR4/priors_for_schemaVR4_20210219_135801.RData")[["prior_schemaVR4_familiar_ind"]];
prior_schemaVR4_familiar_red <- loadToEnv("C:/Users/aq01/Desktop/schemaVR/schemaVR4/priors_for_schemaVR4_20210219_135801.RData")[["prior_schemaVR4_familiar_red"]];

# Creating new labels
remembered <- rep(0, dim(dataSchemaVR4)[1])
remembered[dataSchemaVR4$resCon == 1] <- 1
dataSchemaVR4$remembered <- remembered

# Coding familiar judgements as redundant
familiar_red <- rep(0, dim(dataSchemaVR4)[1])
familiar_red[dataSchemaVR4$resCon == 1] <- 1
familiar_red[dataSchemaVR4$resCon == 2] <- 1
dataSchemaVR4$familiar_red <- familiar_red

# Coding familiar judgements as independent
familiar_ind <- rep(0, dim(dataSchemaVR4)[1])
familiar_ind[dataSchemaVR4$resCon == 1] <- NA_integer_
familiar_ind[dataSchemaVR4$resCon == 2] <- 1
dataSchemaVR4$familiar_ind <- familiar_ind

# Scaling based on Gelman et al. (2008) and https://github.com/stan-dev/stan/wiki/Prior-Choice-Recommendations
# Mean = 0 and SD = 0.5
dataSchemaVR4$Exp  <- dataSchemaVR4$objLocTargetRating 
dataSchemaVR4$sExp <- (dataSchemaVR4$Exp - mean(dataSchemaVR4$Exp ))/(sd(dataSchemaVR4$Exp)/0.5)

# Exclude no-memory (i.e. hasn't seen object) and incorrect trials
dataSchemaVR4_sub <- dataSchemaVR4[dataSchemaVR4$resCon != 0, ]

# /* 
# ----------------------------- Model ---------------------------
# */
model_schemaVR4_rem <- brm(remembered ~ sExp +  I(sExp*sExp)+
                                   (1 | objNum) +
                                   (1 | subNum),
                           data = dataSchemaVR4_sub,
                           prior = prior_schemaVR4_rem,
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
                                    prior = prior_schemaVR4_familiar_red,
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
                                    prior = prior_schemaVR4_familiar_ind,
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
save.image(datedFileNam('schemaVR4_RK', '.RData'))