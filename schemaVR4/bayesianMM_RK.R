######################################################################################
# Script to run Bayesian multilevel models for recollection/familiarity in preparation of schemaVR4
# Version 3.0
# Date: 29/07/2019
# Author: Joern Alexander Quent
######################################################################################
# Setting seed
set.seed(7248446)

# Functions and libaries
library(assortedRFunctions)
library(brms)
library(lmerTest)
library(plyr)

# Loading data
load("U:/Projects/schemaVR/schemaVR3/analysis/dataForBayesianReanalysis.RData") # Windows
#load("/home/aq01/Projects/schemaVR/schemaVR3/analysis/dataForBayesianReanalysis.RData") # Cluster

# General settings
cores2use <- 3

######################################################################################
# Preparation of data
# schemaVR2
schemaVR2_table4 <- ddply(dataSchemaVR2, 
                          c('objNum', 'objName'), 
                          summarise,
                          objLocTargetRating = mean(objLocTargetRating, na.rm = TRUE),
                          RecallNoMemory     = table(recallMemory)[1],
                          RecallRecollection = table(recallMemory)[2],
                          RecallFamiliarity  = table(recallMemory)[3],
                          RecallGuess        = table(recallMemory)[4],
                          RecallN            = sum(table(recallMemory)),
                          RecallRec_per      = (RecallRecollection/RecallN),
                          RecallFam_per      = (RecallFamiliarity/RecallN),
                          AFCNoMemory        = table(resCon)[1], 
                          AFCRecollection    = table(resCon)[2],
                          AFCFamiliarity     = table(resCon)[3],
                          AFCGuess           = table(resCon)[4],
                          AFCN               = sum(table(resCon)),
                          AFCRec_per         = (AFCRecollection/AFCN),
                          AFCFam_per         = (AFCFamiliarity/AFCN),
                          AFCGuess_per       = (AFCGuess/AFCN),
                          AFCFam_indepen     = AFCFam_per/(1-AFCRec_per))
schemaVR2_table4$objLocTargetRating <- scale(schemaVR2_table4$objLocTargetRating)
# The last step is important because otherwise the intercept can be adjust to fit the data better and the 
# beta value even though little overlap with zero is associated with a small BF. The function scale() centres and 
# scales it by the standard deviations. 

# schemaVR3
schemaVR3_table4 <- ddply(dataSchemaVR3, 
                          c('objNum', 'objNam', 'targetLocation'), 
                          summarise,
                          objLocTargetRating = mean(objLocTargetRating, na.rm = TRUE),
                          RecallNoMemory     = table(recallMemory)[1],
                          RecallRecollection = table(recallMemory)[2],
                          RecallFamiliarity  = table(recallMemory)[3],
                          RecallGuess        = table(recallMemory)[4],
                          RecallN            = sum(table(recallMemory)),
                          RecallRec_per      = (RecallRecollection/RecallN),
                          RecallFam_per      = (RecallFamiliarity/RecallN),
                          AFCNoMemory        = table(resCon)[1], 
                          AFCRecollection    = table(resCon)[2],
                          AFCFamiliarity     = table(resCon)[3],
                          AFCGuess           = table(resCon)[4],
                          AFCN               = sum(table(resCon)),
                          AFCRec_per         = (AFCRecollection/AFCN),
                          AFCFam_per         = (AFCFamiliarity/AFCN),
                          AFCFam_indepen     = AFCFam_per/(1-AFCRec_per))
schemaVR3_table4$objLocTargetRating <- scale(schemaVR3_table4$objLocTargetRating)
# The last step is important because otherwise the intercept can be adjust to fit the data better and the 
# beta value even though little overlap with zero is associated with a small BF. The function scale() centres and 
# scales it by the standard deviations.

######################################################################################
# Recollection
# schemaVR2
# Only uniformative priors are used. 
# Run models
schemaVR2_rec_AFC  <- brm(AFCRec_per ~ objLocTargetRating,
                              data = schemaVR2_table4,
                              cores = cores2use,
                              save_all_pars = TRUE,
                              control = list(adapt_delta = 0.95),
                              sample_prior = TRUE)

schemaVR2_rec_AFC_lm <- lm(AFCRec_per ~ objLocTargetRating,
                              data = schemaVR2_table4)

# saving post fixed effect and densities
schemaVR2_rec_AFC_postDen  <- posterior_samples(schemaVR2_rec_AFC)
schemaVR2_rec_AFC_fixef    <- round(fixef(schemaVR2_rec_AFC), 3)
schemaVR2_rec_AFC_ef_lm    <- round(summary(schemaVR2_rec_AFC_lm)$coefficients, 3) 

#############
# schemaVR3
# Setting prior
schemaVR3_rec_AFC_priors <- c(set_prior(priorString_normal(schemaVR2_rec_AFC_fixef[1, 1], schemaVR2_rec_AFC_fixef[1, 2]),
                                       class = "Intercept"),
                              set_prior(priorString_normal(schemaVR2_rec_AFC_fixef[2, 1], schemaVR2_rec_AFC_fixef[2, 2]),
                                       class = "b",
                                       coef = "objLocTargetRating"))

# Run models
schemaVR3_rec_AFC  <- brm(AFCRec_per ~ objLocTargetRating +
                                (1 | objNum) ,
                               data = schemaVR3_table4,
                               cores = cores2use,
                               save_all_pars = TRUE,
                               sample_prior = TRUE,
                               prior = schemaVR3_rec_AFC_priors,
                               control = list(adapt_delta = 0.95))


schemaVR3_rec_AFC_lmer <- lmer(AFCRec_per ~ objLocTargetRating + 
                                 (1 | objNum),
                           data = schemaVR3_table4)

# saving post fixed effect and densities
schemaVR3_rec_AFC_postDen    <- posterior_samples(schemaVR3_rec_AFC)
schemaVR3_rec_AFC_fixef      <- round(fixef(schemaVR3_rec_AFC), 3)
schemaVR3_rec_AFC_fixef_lmer <- round(fixef(schemaVR3_rec_AFC_lmer), 3)
schemaVR3_rec_AFC_ratio      <- hypothesis(schemaVR3_rec_AFC, "objLocTargetRating = 0")$`hypothesis`$Evid.Ratio

######################################################################################
# Familiarity (scored under independence)
# schemaVR2
# Run models
schemaVR2_famInd_AFC  <- brm(AFCFam_indepen ~ objLocTargetRating,
                          data = schemaVR2_table4,
                          cores = cores2use,
                          save_all_pars = TRUE,
                          control = list(adapt_delta = 0.95),
                          sample_prior = TRUE)

schemaVR2_famInd_AFC_lm <- lm(AFCFam_indepen ~ objLocTargetRating,
                           data = schemaVR2_table4)

# saving post fixed effect and densities
schemaVR2_famInd_AFC_postDen  <- posterior_samples(schemaVR2_famInd_AFC)
schemaVR2_famInd_AFC_fixef    <- round(fixef(schemaVR2_famInd_AFC), 3)
schemaVR2_famInd_AFC_ef_lm    <- round(summary(schemaVR2_famInd_AFC_lm)$coefficients, 3) 

#############
# schemaVR3
# Setting prior
schemaVR3_famInd_AFC_priors <- c(set_prior(priorString_normal(schemaVR2_famInd_AFC_fixef[1, 1], schemaVR2_famInd_AFC_fixef[1, 2]),
                                        class = "Intercept"),
                              set_prior(priorString_normal(schemaVR2_famInd_AFC_fixef[2, 1], schemaVR2_famInd_AFC_fixef[2, 2]),
                                        class = "b",
                                        coef = "objLocTargetRating"))

# Run models
schemaVR3_famInd_AFC  <- brm(AFCFam_indepen ~ objLocTargetRating +
                            (1 | objNum) ,
                          data = schemaVR3_table4,
                          cores = cores2use,
                          save_all_pars = TRUE,
                          sample_prior = TRUE,
                          prior = schemaVR3_famInd_AFC_priors,
                          control = list(adapt_delta = 0.95))

schemaVR3_famInd_AFC_lmer <- lmer(AFCFam_indepen ~ objLocTargetRating + 
                                 (1 | objNum),
                               data = schemaVR3_table4)

# saving post fixed effect and densities
schemaVR3_famInd_AFC_postDen    <- posterior_samples(schemaVR3_famInd_AFC)
schemaVR3_famInd_AFC_fixef      <- round(fixef(schemaVR3_famInd_AFC), 3)
schemaVR3_famInd_AFC_fixef_lmer <- round(fixef(schemaVR3_famInd_AFC_lmer), 3)
schemaVR3_famInd_AFC_ratio      <- hypothesis(schemaVR3_famInd_AFC, "objLocTargetRating = 0")$`hypothesis`$Evid.Ratio

######################################################################################
# Saving data 
save.image(datedFileNam('bayesianMM_RK', '.RData'))
