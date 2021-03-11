# Script to get priors for pre-reg for schemaVR4
# Version 1.0
# Date: 19/02/2021
# Author: Joern Alexander Quent
# /* 
# ----------------------------- Libraries, settings and functions ---------------------------
# */

# Libraries
library(assortedRFunctions)
library(brms)
library(plyr)
library(beepr)

# General settings
cores2use <- 4

# /* 
# ----------------------------- Recall ---------------------------
# */
load("C:/Users/aq01/Desktop/schemaVR/schemaVR3/analysis/schemaVR3_recall_20210213_190857.RData")

postDists                 <- posterior_samples(model_schemaVR3_recall)
# Intercept
intercept_schemaVR_recall <- brm(b_Intercept ~ 1,
                                 data = postDists,
                                 cores = cores2use,
                                 family = student(link = "identity", link_sigma = "log", link_nu = "logm1"))

# Check, beep and sleep for 10 sec
pp_check(intercept_schemaVR_recall)
beep(8)
Sys.sleep(10)

# Linear term
b_sExp_schemaVR_recall <- brm(b_sExp ~ 1,
                              data = postDists,
                              cores = cores2use,
                              family = student(link = "identity", link_sigma = "log", link_nu = "logm1"))#

# Check, beep and sleep for 10 sec
pp_check(b_sExp_schemaVR_recall)
beep(8)
Sys.sleep(10)


b_IsExpMUsExp_schemaVR_recall <- brm(b_IsExpMUsExp ~ 1,
                                     data = postDists,
                                     cores = cores2use,
                                     family = student(link = "identity", link_sigma = "log", link_nu = "logm1"))

# Check, beep and sleep for 10 sec
pp_check(b_IsExpMUsExp_schemaVR_recall)
beep(8)
Sys.sleep(10)


# Make prior
prior_schemaVR4_recall  <- c(set_prior(priorString_student(intercept_schemaVR_recall), 
                                class = "Intercept"),
                            set_prior(priorString_student(b_sExp_schemaVR_recall), 
                                class = "b", 
                                coef = "sExp"),
                            set_prior(priorString_student(b_IsExpMUsExp_schemaVR_recall), 
                                class = "b", 
                                coef = "IsExpMUsExp"))


# /* 
# ----------------------------- 3AFC ---------------------------
# */
load("C:/Users/aq01/Desktop/schemaVR/schemaVR3/analysis/schemaVR3_AFC_20210219_092010.RData")

postDists                 <- posterior_samples(model_schemaVR3_AFC)
# Intercept
intercept_schemaVR_AFC <- brm(b_Intercept ~ 1,
                                 data = postDists,
                                 cores = cores2use,
                                 family = student(link = "identity", link_sigma = "log", link_nu = "logm1"))

# Check, beep and sleep for 10 sec
pp_check(intercept_schemaVR_AFC)
beep(8)
Sys.sleep(10)

# Linear term
b_sExp_schemaVR_AFC <- brm(b_sExp ~ 1,
                              data = postDists,
                              cores = cores2use,
                              family = student(link = "identity", link_sigma = "log", link_nu = "logm1"))#

# Check, beep and sleep for 10 sec
pp_check(b_sExp_schemaVR_AFC)
beep(8)
Sys.sleep(10)


b_IsExpMUsExp_schemaVR_AFC <- brm(b_IsExpMUsExp ~ 1,
                                     data = postDists,
                                     cores = cores2use,
                                     family = student(link = "identity", link_sigma = "log", link_nu = "logm1"))

# Check, beep and sleep for 10 sec
pp_check(b_IsExpMUsExp_schemaVR_AFC)
beep(8)
Sys.sleep(10)

prior_schemaVR4_AFC  <- c(set_prior(priorString_student(intercept_schemaVR_AFC), 
                                       class = "Intercept"),
                             set_prior(priorString_student(b_sExp_schemaVR_AFC), 
                                       class = "b", 
                                       coef = "sExp"),
                             set_prior(priorString_student(b_IsExpMUsExp_schemaVR_AFC), 
                                       class = "b", 
                                       coef = "IsExpMUsExp"))


# /* 
# ----------------------------- Euclidean distance ---------------------------
# */
load("C:/Users/aq01/Desktop/schemaVR/schemaVR3/analysis/schemaVR3_euclid_20210213_195542.RData")

postDists                 <- posterior_samples(model_schemaVR3_euclid)
# Intercept
intercept_schemaVR_euclid <- brm(b_Intercept ~ 1,
                              data = postDists,
                              cores = cores2use,
                              family = student(link = "identity", link_sigma = "log", link_nu = "logm1"))

# Check, beep and sleep for 10 sec
pp_check(intercept_schemaVR_euclid)
beep(8)
Sys.sleep(10)

# Linear term
b_sExp_schemaVR_euclid <- brm(b_sExp ~ 1,
                           data = postDists,
                           cores = cores2use,
                           family = student(link = "identity", link_sigma = "log", link_nu = "logm1"))#

# Check, beep and sleep for 10 sec
pp_check(b_sExp_schemaVR_euclid)
beep(8)
Sys.sleep(10)


b_IsExpMUsExp_schemaVR_euclid <- brm(b_IsExpMUsExp ~ 1,
                                  data = postDists,
                                  cores = cores2use,
                                  family = student(link = "identity", link_sigma = "log", link_nu = "logm1"))

# Check, beep and sleep for 10 sec
pp_check(b_IsExpMUsExp_schemaVR_euclid)
beep(8)
Sys.sleep(10)

prior_schemaVR4_euclid  <- c(set_prior(priorString_student(intercept_schemaVR_euclid), 
                                    class = "Intercept"),
                          set_prior(priorString_student(b_sExp_schemaVR_euclid), 
                                    class = "b", 
                                    coef = "sExp"),
                          set_prior(priorString_student(b_IsExpMUsExp_schemaVR_euclid), 
                                    class = "b", 
                                    coef = "IsExpMUsExp"))

# /* 
# ----------------------------- Remember ---------------------------
# */
load("C:/Users/aq01/Desktop/schemaVR/schemaVR3/analysis/schemaVR3_RK_20210219_103034.RData")

postDists                 <- posterior_samples(model_schemaVR3_rem)
# Intercept
intercept_schemaVR_rem <- brm(b_Intercept ~ 1,
                                 data = postDists,
                                 cores = cores2use,
                                 family = student(link = "identity", link_sigma = "log", link_nu = "logm1"))

# Check, beep and sleep for 10 sec
pp_check(intercept_schemaVR_rem)
beep(8)
Sys.sleep(10)

# Linear term
b_sExp_schemaVR_rem <- brm(b_sExp ~ 1,
                              data = postDists,
                              cores = cores2use,
                              family = student(link = "identity", link_sigma = "log", link_nu = "logm1"))#

# Check, beep and sleep for 10 sec
pp_check(b_sExp_schemaVR_rem)
beep(8)
Sys.sleep(10)


b_IsExpMUsExp_schemaVR_rem <- brm(b_IsExpMUsExp ~ 1,
                                     data = postDists,
                                     cores = cores2use,
                                     family = student(link = "identity", link_sigma = "log", link_nu = "logm1"))

# Check, beep and sleep for 10 sec
pp_check(b_IsExpMUsExp_schemaVR_rem)
beep(8)
Sys.sleep(10)

prior_schemaVR4_rem  <- c(set_prior(priorString_student(intercept_schemaVR_rem), 
                                    class = "Intercept"),
                          set_prior(priorString_student(b_sExp_schemaVR_rem), 
                                    class = "b", 
                                    coef = "sExp"),
                          set_prior(priorString_student(b_IsExpMUsExp_schemaVR_rem), 
                                    class = "b", 
                                    coef = "IsExpMUsExp"))

# /* 
# ----------------------------- Familiarity independence ---------------------------
# */
postDists                 <- posterior_samples(model_schemaVR3_familiar_ind)
# Intercept
intercept_schemaVR_familiar_ind <- brm(b_Intercept ~ 1,
                              data = postDists,
                              cores = cores2use,
                              family = student(link = "identity", link_sigma = "log", link_nu = "logm1"))

# Check, beep and sleep for 10 sec
pp_check(intercept_schemaVR_familiar_ind)
beep(8)
Sys.sleep(10)

# Linear term
b_sExp_schemaVR_familiar_ind <- brm(b_sExp ~ 1,
                           data = postDists,
                           cores = cores2use,
                           family = student(link = "identity", link_sigma = "log", link_nu = "logm1"))#

# Check, beep and sleep for 10 sec
pp_check(b_sExp_schemaVR_familiar_ind)
beep(8)
Sys.sleep(10)


b_IsExpMUsExp_schemaVR_familiar_ind <- brm(b_IsExpMUsExp ~ 1,
                                  data = postDists,
                                  cores = cores2use,
                                  family = student(link = "identity", link_sigma = "log", link_nu = "logm1"))

# Check, beep and sleep for 10 sec
pp_check(b_IsExpMUsExp_schemaVR_familiar_ind)
beep(8)
Sys.sleep(10)

prior_schemaVR4_familiar_ind  <- c(set_prior(priorString_student(intercept_schemaVR_familiar_ind), 
                                    class = "Intercept"),
                          set_prior(priorString_student(b_sExp_schemaVR_familiar_ind), 
                                    class = "b", 
                                    coef = "sExp"),
                          set_prior(priorString_student(b_IsExpMUsExp_schemaVR_familiar_ind), 
                                    class = "b", 
                                    coef = "IsExpMUsExp"))
# /* 
# ----------------------------- Familiarity redundancy ---------------------------
# */

postDists                 <- posterior_samples(model_schemaVR3_familiar_red)
# Intercept
intercept_schemaVR_familiar_red <- brm(b_Intercept ~ 1,
                                       data = postDists,
                                       cores = cores2use,
                                       family = student(link = "identity", link_sigma = "log", link_nu = "logm1"))

# Check, beep and sleep for 10 sec
pp_check(intercept_schemaVR_familiar_red)
beep(8)
Sys.sleep(10)

# Linear term
b_sExp_schemaVR_familiar_red <- brm(b_sExp ~ 1,
                                    data = postDists,
                                    cores = cores2use,
                                    family = student(link = "identity", link_sigma = "log", link_nu = "logm1"))#

# Check, beep and sleep for 10 sec
pp_check(b_sExp_schemaVR_familiar_red)
beep(8)
Sys.sleep(10)


b_IsExpMUsExp_schemaVR_familiar_red <- brm(b_IsExpMUsExp ~ 1,
                                           data = postDists,
                                           cores = cores2use,
                                           family = student(link = "identity", link_sigma = "log", link_nu = "logm1"))

# Check, beep and sleep for 10 sec
pp_check(b_IsExpMUsExp_schemaVR_familiar_red)
beep(8)
Sys.sleep(10)

prior_schemaVR4_familiar_red  <- c(set_prior(priorString_student(intercept_schemaVR_familiar_red), 
                                             class = "Intercept"),
                                   set_prior(priorString_student(b_sExp_schemaVR_familiar_red), 
                                             class = "b", 
                                             coef = "sExp"),
                                   set_prior(priorString_student(b_IsExpMUsExp_schemaVR_familiar_red), 
                                             class = "b", 
                                             coef = "IsExpMUsExp"))

# /* 
# ----------------------------- Saving image ---------------------------
# */
save.image(datedFileNam('priors_for_schemaVR4', '.RData'))