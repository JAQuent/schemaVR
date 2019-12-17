######################################################################################
# Script to run Bayesian multilevel models for Recall accuracy in preparation of schemaVR4
# Version 2.0
# Date: 24/07/2019
# Author: Joern Alexander Quent
######################################################################################
# Setting seed
set.seed(724846)

# Functions and libaries
library(assortedRFunctions)
library(brms)
library(lmerTest)

# Loading data
load("U:/Projects/schemaVR/schemaVR3/analysis/dataForBayesianReanalysis.RData") # Windows
#load("/home/aq01/Projects/schemaVR/schemaVR3/analysis/dataForBayesianReanalysis.RData") # Cluster

# General settings
cores2use <- 3

######################################################################################
# SchemaVR1
# Only uniformative priors are used. 
# Running both models
schemaVR1_recall <- brm(accRecall ~  objLocTargetRating +  
                                      I(objLocTargetRating*objLocTargetRating) +
                                      (1 | subNum) +
                                      (1 | objNum),
                                    data = dataSchemaVR1_recall,
                                    cores = cores2use,
                                    family = bernoulli(),
                                    save_all_pars = TRUE)

schemaVR1_recall_lmer <- glmer(accRecall ~  objLocTargetRating +
                                      I(objLocTargetRating*objLocTargetRating) + 
                                      (1 | subNum) +
                                      (1 | objNum),
                                    data = dataSchemaVR1_recall,
                                    family = binomial,
                                    control = glmerControl(optimizer = "bobyqa"),
                                    nAGQ = 1)


# saving post fixed effect and densities
schemaVR1_recall_postDen    <- posterior_samples(schemaVR1_recall)
schemaVR1_recall_fixef      <- round(fixef(schemaVR1_recall), 3)
schemaVR1_recall_fixef_lmer <- round(fixef(schemaVR1_recall_lmer), 3)

#########################################################################
# SchemaVR2
#############
# Setting priors
schemaVR2_recall_priors <- c(set_prior(priorString_normal(schemaVR1_recall_fixef[1, 1], schemaVR1_recall_fixef[1, 2]),
                                       class = "Intercept"),
                             set_prior(priorString_normal(schemaVR1_recall_fixef[2, 1], schemaVR1_recall_fixef[2, 2]),
                                       class = "b",
                                       coef = "objLocTargetRating"),
                             set_prior(priorString_normal(schemaVR1_recall_fixef[3, 1], schemaVR1_recall_fixef[3, 2]),
                                       lass = "b",
                                       coef = "IobjLocTargetRatingMUobjLocTargetRating"))

# Running model
schemaVR2_recall <- brm(accRecall ~ objLocTargetRating +  
                                      I(objLocTargetRating*objLocTargetRating) +
                                      (1 | subNum) +
                                      (1 | objNum),
                                    data = dataSchemaVR2_recall,
                                    cores = cores2use,
                                    family = bernoulli(),
                                    save_all_pars = TRUE,
                                    sample_prior = TRUE,
                                    prior = schemaVR2_recall_priors)

schemaVR2_recall_lmer <- glmer(accRecall ~  objLocTargetRating +
                                 I(objLocTargetRating*objLocTargetRating) + 
                                 (1 | subNum) +
                                 (1 | objNum),
                               data = dataSchemaVR2_recall,
                               family = binomial,
                               control = glmerControl(optimizer = "bobyqa"),
                               nAGQ = 1)


# saving post fixed effect, densities, Evidence ratios
schemaVR2_recall_postDen    <- posterior_samples(schemaVR2_recall)
schemaVR2_recall_fixef      <- round(fixef(schemaVR2_recall), 3)
schemaVR2_recall_fixef_lmer <- round(fixef(schemaVR2_recall_lmer), 3) # Control that estimate
schemaVR2_recall_ratio      <- hypothesis(schemaVR2_recall, "IobjLocTargetRatingMUobjLocTargetRating = 0")$`hypothesis`$Evid.Ratio

plot(marginal_effects(schemaVR2_recall, effects = "objLocTargetRating")) 


######################################################################################
# SchemaVR3
#############
# Setting priors
schemaVR3_recall_priors <- c(set_prior(priorString_normal(schemaVR2_recall_fixef[1, 1], schemaVR2_recall_fixef[1, 2]),
                                       class = "Intercept"),
                             set_prior(priorString_normal(schemaVR2_recall_fixef[2, 1], schemaVR2_recall_fixef[2, 2]),
                                       class = "b",
                                       coef = "objLocTargetRating"),
                             set_prior(priorString_normal(schemaVR2_recall_fixef[3, 1], schemaVR2_recall_fixef[3, 2]),
                                       class = "b",
                                       coef = "IobjLocTargetRatingMUobjLocTargetRating"))

# Running model
schemaVR3_recall <- brm(accRecall ~ objLocTargetRating +  
                                      I(objLocTargetRating*objLocTargetRating) +
                                      (1 | subNum) +
                                      (1 | objNum),
                                    data = dataSchemaVR3_recall,
                                    cores = cores2use,
                                    family = bernoulli(),
                                    save_all_pars = TRUE,
                                    sample_prior = TRUE,
                                    prior = schemaVR3_recall_priors)

schemaVR3_recall_lmer <- glmer(accRecall ~  objLocTargetRating +
                                 I(objLocTargetRating*objLocTargetRating) + 
                                 (1 | subNum) +
                                 (1 | objNum),
                               data = dataSchemaVR3_recall,
                               family = binomial,
                               control = glmerControl(optimizer = "bobyqa"),
                               nAGQ = 1)


# saving post fixed effect, densities, Evidence ratios
schemaVR3_recall_postDen    <- posterior_samples(schemaVR3_recall)
schemaVR3_recall_fixef      <- round(fixef(schemaVR3_recall), 3)
schemaVR3_recall_fixef_lmer <- round(fixef(schemaVR3_recall_lmer), 3) # Control that estimate
schemaVR3_recall_ratio      <- hypothesis(schemaVR3_recall, "IobjLocTargetRatingMUobjLocTargetRating = 0")$`hypothesis`$Evid.Ratio

plot(marginal_effects(schemaVR3_recall, effects = "objLocTargetRating")) 

######################################################################################
# Saving data 
save.image(datedFileNam('bayesianMM_recall', '.RData'))
