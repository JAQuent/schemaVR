# Script to run analysis of r/k data from schemaVR3
# Version 1.0
# Date: 16/10/2019
# Author: Joern Alexander Quent
# /* 
# ----------------------------- Libraries, settings and functions ---------------------------
# */
# Setting seed
seed <- 238
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
load("U:/Projects/schemaVR/schemaVR3/data/dataSchemaVR3_cleaned.RData")
load("U:/Projects/schemaVR/schemaVR2/analysis/schemaVR2_RK_20191015_154626.RData")

# Creating new labels
remembered <- rep(0, dim(dataSchemaVR3)[1])
remembered[dataSchemaVR3$resCon == 1] <- 1
dataSchemaVR3$remembered <- remembered

# Coding familiar judgments as redundant
familiar_red <- rep(0, dim(dataSchemaVR3)[1])
familiar_red[dataSchemaVR3$resCon == 1] <- 1
familiar_red[dataSchemaVR3$resCon == 2] <- 1
dataSchemaVR3$familiar_red <- familiar_red

# Coding familiar judgments as independent
familiar_ind <- rep(0, dim(dataSchemaVR3)[1])
familiar_ind[dataSchemaVR3$resCon == 1] <- NA_integer_
familiar_ind[dataSchemaVR3$resCon == 2] <- 1
dataSchemaVR3$familiar_ind <- familiar_ind

# Scaling based on Gelman et al. (2008) and https://github.com/stan-dev/stan/wiki/Prior-Choice-Recommendations
# Mean = 0 and SD = 0.5
dataSchemaVR3$Exp  <- dataSchemaVR3$objLocTargetRating 
dataSchemaVR3$sExp <- (dataSchemaVR3$Exp - mean(dataSchemaVR3$Exp ))/(sd(dataSchemaVR3$Exp)/0.5)

# Exclude no-memory (i.e. hasn't seen object) and incorrect trials
dataSchemaVR3_sub <- dataSchemaVR3[dataSchemaVR3$resCon != 0 & dataSchemaVR3$accAFC == 1, ]

# /* 
# ----------------------------- Priors ---------------------------
# */
# Remember
postDists                      <- posterior_samples(model_schemaVR2_rem)
intercept_schemaVR2_RK_rem     <- brm(b_Intercept ~ 1,
                                      data = postDists,
                                      family = student(link = "identity", link_sigma = "log", link_nu = "logm1"))
b_sExp_schemaVR2_RK_rem        <- brm(b_sExp ~ 1,
                                      data = postDists,
                                      family = student(link = "identity", link_sigma = "log", link_nu = "logm1"))
b_IsExpMUsExp_schemaVR2_RK_rem <- brm(b_IsExpMUsExp ~ 1,
                                      data = postDists,
                                      family = student(link = "identity", link_sigma = "log", link_nu = "logm1"))


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
                                          family = student(link = "identity", link_sigma = "log", link_nu = "logm1"))
b_sExp_schemaVR2_RK_fam_red        <- brm(b_sExp ~ 1,
                                          data = postDists,
                                          family = student(link = "identity", link_sigma = "log", link_nu = "logm1"))
b_IsExpMUsExp_schemaVR2_RK_fam_red <- brm(b_IsExpMUsExp ~ 1,
                                          data = postDists,
                                          family = student(link = "identity", link_sigma = "log", link_nu = "logm1"))

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
                                          family = student(link = "identity", link_sigma = "log", link_nu = "logm1"))
b_sExp_schemaVR2_RK_fam_ind        <- brm(b_sExp ~ 1,
                                          data = postDists,
                                          family = student(link = "identity", link_sigma = "log", link_nu = "logm1"))
b_IsExpMUsExp_schemaVR2_RK_fam_ind <- brm(b_IsExpMUsExp ~ 1,
                                          data = postDists,
                                          family = student(link = "identity", link_sigma = "log", link_nu = "logm1"))

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
                           cores = cores2use,
                           save_all_pars = TRUE,
                           sample_prior = TRUE,
                           seed = seeds[1])

model_schemaVR3_familiar_red  <- brm(familiar_red ~ sExp +  I(sExp*sExp)+
                                    (1 | objNum) +
                                    (1 | subNum),
                                    data = dataSchemaVR3_sub,
                                    prior = prior_schemaVR3_fam_red,
                                    family = bernoulli(),
                                    cores = cores2use,
                                    save_all_pars = TRUE,
                                    sample_prior = TRUE,
                                    seed = seeds[2])


model_schemaVR3_familiar_ind  <- brm(familiar_ind ~ sExp +  I(sExp*sExp)+
                                    (1 | objNum) +
                                    (1 | subNum),
                                    data = dataSchemaVR3_sub,
                                    prior = prior_schemaVR3_fam_ind,
                                    family = bernoulli(),
                                    cores = cores2use,
                                    save_all_pars = TRUE,
                                    sample_prior = TRUE,
                                    seed = seeds[3])

# /* 
# ----------------------------- Saving iamge ---------------------------
# */
save.image(datedFileNam('schemaVR3_RK', '.RData'))