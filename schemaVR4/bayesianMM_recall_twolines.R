# Script to run Bayesian multilevel models for recollection/familiarity in preparation of schemaVR4
# Version 2.0
# Date: 29/07/2019
# Author: Joern Alexander Quent
# /* 
# ----------------------------- General stuff ---------------------------
# */
# Functions and libaries
library(assortedRFunctions)
library(brms)
library(lmerTest)

# Loading data
load("U:/Projects/schemaVR/schemaVR3/analysis/dataForBayesianReanalysis.RData")

# General settings
cores2use <- 3

# Breaking point based on calculations of quadtratic terms of the BRMS models
brPoint <- -0.2965426

# /* 
# ----------------------------- schemaVR1 ---------------------------
# */
# Creating variables for interrupted regression
# Fitting only one model for both lines because it doesn't make a difference for this data set.
dataSchemaVR1_recall_twolines <- dataSchemaVR1_recall
schemaVR1_brPo             <- brPoint 
xu                         <- dataSchemaVR1_recall_twolines$objLocTargetRating
dataSchemaVR1_recall_twolines$xlow  <- ifelse(xu <= schemaVR1_brPo, xu - schemaVR1_brPo, 0)
dataSchemaVR1_recall_twolines$xhigh <- ifelse(xu > schemaVR1_brPo, xu - schemaVR1_brPo, 0)     
dataSchemaVR1_recall_twolines$high  <- ifelse(xu > schemaVR1_brPo, 1, 0)

# Running both models
# Setting priors
schemaVR1_recall_twolines_priors <- c(set_prior(priorString_normal(0.75,1),
                                                class = "Intercept"),
                                      set_prior(priorString_normal(0, 1),
                                                class = "b",
                                                coef = "high"),
                                      set_prior(priorString_normal(0, 1),
                                                class = "b",
                                                coef = "xhigh"),
                                      set_prior(priorString_normal(0, 1),
                                                class = "b",
                                                coef = "xlow"))

schemaVR1_recall_twolines <- brm(accRecall ~  xlow + xhigh + high +
                             (1 | subNum) +
                             (1 | objNum),
                           data = dataSchemaVR1_recall_twolines,
                           prior = schemaVR1_recall_twolines_priors,
                           cores = cores2use,
                           family = bernoulli(),
                           save_all_pars = TRUE,
                           sample_prior = TRUE)
hypothesis(schemaVR1_recall_twolines, "xhigh = 0")


schemaVR1_recall_twolines_lmer <- glmer(accRecall ~  xlow + xhigh + high +
                                   (1 | subNum) +
                                   (1 | objNum),
                                   data = dataSchemaVR1_recall_twolines,
                                   family = binomial,
                                   control = glmerControl(optimizer = "bobyqa"),
                                   nAGQ = 1)

# saving post fixed effect and densities
schemaVR1_recall_twolines_postDen    <- posterior_samples(schemaVR1_recall_twolines)
schemaVR1_recall_twolines_fixef      <- round(fixef(schemaVR1_recall_twolines), 3)
schemaVR1_recall_twolines_fixef_lmer <- round(fixef(schemaVR1_recall_twolines_lmer), 3)



# /* 
# ----------------------------- schemaVR2 ---------------------------
# */
# Creating variables for interrupted regression
# Fitting only one model for both lines because it doesn't make a difference for this data set.
dataSchemaVR2_recall_twolines <- dataSchemaVR2_recall
schemaVR2_brPo             <- brPoint 
xu                         <- dataSchemaVR2_recall_twolines$objLocTargetRating
dataSchemaVR2_recall_twolines$xlow  <- ifelse(xu <= schemaVR2_brPo, xu - schemaVR2_brPo, 0)
dataSchemaVR2_recall_twolines$xhigh <- ifelse(xu > schemaVR2_brPo, xu - schemaVR2_brPo, 0)     
dataSchemaVR2_recall_twolines$high  <- ifelse(xu > schemaVR2_brPo, 1, 0)


# Setting priors
schemaVR2_recall_twolines_priors <- c(set_prior(priorString_normal(schemaVR1_recall_twolines_fixef[1, 1], 
                                                                   schemaVR1_recall_twolines_fixef[1, 2]),
                                       class = "Intercept"),
                                      set_prior(priorString_normal(schemaVR1_recall_twolines_fixef[4, 1], 
                                                                   schemaVR1_recall_twolines_fixef[4, 2]),
                                       class = "b",
                                       coef = "high"),
                                      set_prior(priorString_normal(schemaVR1_recall_twolines_fixef[3, 1], 
                                                                   schemaVR1_recall_twolines_fixef[3, 2]),
                                       class = "b",
                                       coef = "xhigh"),
                                      set_prior(priorString_normal(schemaVR1_recall_twolines_fixef[2, 1], 
                                                                   schemaVR1_recall_twolines_fixef[2, 2]),
                                       class = "b",
                                       coef = "xlow"))

# Running both models
schemaVR2_recall_twolines <- brm(accRecall ~  xlow + xhigh + high +
                                   (1 | subNum) +
                                   (1 | objNum),
                                 data = dataSchemaVR2_recall_twolines,
                                 cores = cores2use,
                                 family = bernoulli(),
                                 save_all_pars = TRUE,
                                 sample_prior = TRUE, 
                                 prior = schemaVR2_recall_twolines_priors)

schemaVR2_recall_twolines_lmer <- glmer(accRecall ~  xlow + xhigh + high +
                                          (1 | subNum) +
                                          (1 | objNum),
                                        data = dataSchemaVR2_recall_twolines,
                                        family = binomial,
                                        control = glmerControl(optimizer = "bobyqa"),
                                        nAGQ = 1)

# saving post fixed effect and densities
schemaVR2_recall_twolines_postDen    <- posterior_samples(schemaVR2_recall_twolines)
schemaVR2_recall_twolines_fixef      <- round(fixef(schemaVR2_recall_twolines), 3)
schemaVR2_recall_twolines_fixef_lmer <- round(fixef(schemaVR2_recall_twolines_lmer), 3)
schemaVR2_recall_twolines_ratio_low  <- hypothesis(schemaVR2_recall_twolines, "xlow = 0")$`hypothesis`$Evid.Ratio
schemaVR2_recall_twolines_ratio_high <- hypothesis(schemaVR2_recall_twolines, "xhigh = 0")$`hypothesis`$Evid.Ratio

# /* 
# ----------------------------- schemaVR3 ---------------------------
# */
# Creating variables for interrupted regression
# Fitting only one model for both lines because it doesn't make a difference for this data set.
dataSchemaVR3_recall_twolines <- dataSchemaVR3_recall
schemaVR3_brPo             <- brPoint 
xu                         <- dataSchemaVR3_recall_twolines$objLocTargetRating
dataSchemaVR3_recall_twolines$xlow  <- ifelse(xu <= schemaVR3_brPo, xu - schemaVR3_brPo, 0)
dataSchemaVR3_recall_twolines$xhigh <- ifelse(xu > schemaVR3_brPo, xu - schemaVR3_brPo, 0)     
dataSchemaVR3_recall_twolines$high  <- ifelse(xu > schemaVR3_brPo, 1, 0)


# Setting priors
schemaVR3_recall_twolines_priors <- c(set_prior(priorString_normal(schemaVR2_recall_twolines_fixef[1, 1], 
                                                                   schemaVR2_recall_twolines_fixef[1, 2]),
                                                class = "Intercept"),
                                      set_prior(priorString_normal(schemaVR2_recall_twolines_fixef[4, 1], 
                                                                   schemaVR2_recall_twolines_fixef[4, 2]),
                                                class = "b",
                                                coef = "high"),
                                      set_prior(priorString_normal(schemaVR2_recall_twolines_fixef[3, 1], 
                                                                   schemaVR2_recall_twolines_fixef[3, 2]),
                                                class = "b",
                                                coef = "xhigh"),
                                      set_prior(priorString_normal(schemaVR2_recall_twolines_fixef[2, 1], 
                                                                   schemaVR2_recall_twolines_fixef[2, 2]),
                                                class = "b",
                                                coef = "xlow"))

# Running both models
schemaVR3_recall_twolines <- brm(accRecall ~  xlow + xhigh + high +
                                   (1 | subNum) +
                                   (1 | objNum),
                                 data = dataSchemaVR3_recall_twolines,
                                 cores = cores2use,
                                 family = bernoulli(),
                                 save_all_pars = TRUE,
                                 sample_prior = TRUE, 
                                 prior = schemaVR3_recall_twolines_priors)

schemaVR3_recall_twolines_lmer <- glmer(accRecall ~  xlow + xhigh + high +
                                          (1 | subNum) +
                                          (1 | objNum),
                                        data = dataSchemaVR3_recall_twolines,
                                        family = binomial,
                                        control = glmerControl(optimizer = "bobyqa"),
                                        nAGQ = 1)

# saving post fixed effect and densities
schemaVR3_recall_twolines_postDen    <- posterior_samples(schemaVR3_recall_twolines)
schemaVR3_recall_twolines_fixef      <- round(fixef(schemaVR3_recall_twolines), 3)
schemaVR3_recall_twolines_fixef_lmer <- round(fixef(schemaVR3_recall_twolines_lmer), 3)
schemaVR3_recall_twolines_ratio_low  <- hypothesis(schemaVR3_recall_twolines, "xlow = 0")$`hypothesis`$Evid.Ratio
schemaVR3_recall_twolines_ratio_high <- hypothesis(schemaVR3_recall_twolines, "xhigh = 0")$`hypothesis`$Evid.Ratio


# /* 
# ----------------------------- Saving data ---------------------------
# */
save.image(datedFileNam('bayesianMM_recall_twolines', '.RData'))
