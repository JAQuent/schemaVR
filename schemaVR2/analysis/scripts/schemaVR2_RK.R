# Script to run analysis of r/k data from schemaVR2
# Version 1.0
# Date: 15/10/2019
# Author: Joern Alexander Quent
# /* 
# ----------------------------- Libraries, settings and functions ---------------------------
# */
# Setting seed
seed <- 33846
set.seed(seed)
seeds <- sample(1:9999, 3)

# Libaries
library(assortedRFunctions)
library(brms)
library(plyr)

# General settings
cores2use <- 4

# /* 
# ----------------------------- Loeading and preparing data ---------------------------
# */
load("U:/Projects/schemaVR/schemaVR2/data/dataSchemaVR2_cleaned.RData")

# Creating new labels
remembered <- rep(0, dim(dataSchemaVR2)[1])
remembered[dataSchemaVR2$resCon == 1] <- 1
dataSchemaVR2$remembered <- remembered

# Coding familiar judgments as redundant
familiar_red <- rep(0, dim(dataSchemaVR2)[1])
familiar_red[dataSchemaVR2$resCon == 1] <- 1
familiar_red[dataSchemaVR2$resCon == 2] <- 1
dataSchemaVR2$familiar_red <- familiar_red

# Coding familiar judgments as independent
familiar_ind <- rep(0, dim(dataSchemaVR2)[1])
familiar_ind[dataSchemaVR2$resCon == 1] <- NA_integer_
familiar_ind[dataSchemaVR2$resCon == 2] <- 1
dataSchemaVR2$familiar_ind <- familiar_ind

# Scaling based on Gelman et al. (2008) and https://github.com/stan-dev/stan/wiki/Prior-Choice-Recommendations
# Mean = 0 and SD = 0.5
dataSchemaVR2$Exp  <- dataSchemaVR2$objLocTargetRating 
dataSchemaVR2$sExp <- (dataSchemaVR2$Exp - mean(dataSchemaVR2$Exp ))/(sd(dataSchemaVR2$Exp)/0.5)

# Exclude no-memory (i.e. hasn't seen object) and incorrect trials
dataSchemaVR2_sub <- dataSchemaVR2[dataSchemaVR2$resCon != 0 & dataSchemaVR2$accAFC == 1, ]

# /* 
# ----------------------------- Prior ---------------------------
# */
# Based on Gelman et al. (2008) and https://github.com/stan-dev/stan/wiki/Prior-Choice-Recommendations
prior_schemaVR2  <- c(prior(student_t(3, 0, 10) , class = "Intercept"),
                      prior(student_t(3, 0, 2.5) , class = "b")) 

# /* 
# ----------------------------- Model ---------------------------
# */
model_schemaVR2_rem <- brm(remembered ~ sExp +  I(sExp*sExp)+
                                   (1 | objNum) +
                                   (1 | subNum),
                           data = dataSchemaVR2_sub,
                           prior = prior_schemaVR2,
                           family = bernoulli(),
                           cores = cores2use,
                           save_all_pars = TRUE,
                           sample_prior = TRUE,
                           seed = seeds[1])

model_schemaVR2_familiar_red  <- brm(familiar_red ~ sExp +  I(sExp*sExp)+
                                    (1 | objNum) +
                                    (1 | subNum),
                                    data = dataSchemaVR2_sub,
                                    prior = prior_schemaVR2,
                                    family = bernoulli(),
                                    cores = cores2use,
                                    save_all_pars = TRUE,
                                    sample_prior = TRUE,
                                    seed = seeds[2])


model_schemaVR2_familiar_ind  <- brm(familiar_ind ~ sExp +  I(sExp*sExp)+
                                    (1 | objNum) +
                                    (1 | subNum),
                                    data = dataSchemaVR2_sub,
                                    prior = prior_schemaVR2,
                                    family = bernoulli(),
                                    cores = cores2use,
                                    save_all_pars = TRUE,
                                    sample_prior = TRUE,
                                    seed = seeds[3])

# /* 
# ----------------------------- Saving iamge ---------------------------
# */
save.image(datedFileNam('schemaVR2_RK', '.RData'))