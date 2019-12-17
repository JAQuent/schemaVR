# Script to run analysis of recollection data for power analsys with experiment as random factor
# Version 1.0
# Date: 10/09/2019
# Author: Joern Alexander Quent
# /* 
# ----------------------------- Libraries, settings and functions ---------------------------
# */
# Setting seed
seed <- 7243846
set.seed(seed)
seeds <- sample(1:9999, 2)

# Libaries
library(assortedRFunctions)
library(brms)
library(plyr)

# General settings
cores2use <- 4

# /* 
# ----------------------------- Preparing data ---------------------------
# */
# Loading data
load("U:/Projects/schemaVR/schemaVR3/analysis/dataForBayesianReanalysis.RData") 

# schemaVR2
schemaVR2_table <- ddply(dataSchemaVR2, 
                          c('objNum', 'objName'), 
                          summarise,
                          objLocTargetRating = mean(objLocTargetRating, na.rm = TRUE),
                          AFCNoMemory        = table(resCon)[1], 
                          AFCRecollection    = table(resCon)[2],
                          AFCFamiliarity     = table(resCon)[3],
                          AFCGuess           = table(resCon)[4],
                          AFCN               = sum(table(resCon)),
                          AFCRec_per         = (AFCRecollection/AFCN),
                          AFCFam_per         = (AFCFamiliarity/AFCN),
                          AFCGuess_per       = (AFCGuess/AFCN),
                          AFCFam_indepen     = AFCFam_per/(1-AFCRec_per))
schemaVR2_table$objLocTargetRating <- scale(schemaVR2_table$objLocTargetRating)

# schemaVR3
schemaVR3_table <- ddply(dataSchemaVR3, 
                          c('objNum', 'objNam', 'targetLocation'), 
                          summarise,
                          objLocTargetRating = mean(objLocTargetRating, na.rm = TRUE),
                          AFCNoMemory        = table(resCon)[1], 
                          AFCRecollection    = table(resCon)[2],
                          AFCFamiliarity     = table(resCon)[3],
                          AFCGuess           = table(resCon)[4],
                          AFCN               = sum(table(resCon)),
                          AFCRec_per         = (AFCRecollection/AFCN),
                          AFCFam_per         = (AFCFamiliarity/AFCN),
                          AFCFam_indepen     = AFCFam_per/(1-AFCRec_per))
schemaVR3_table$objLocTargetRating <- scale(schemaVR3_table$objLocTargetRating)


# Concatenating data
combinedData <- data.frame(objNum = c(schemaVR2_table$objNum,
                                      schemaVR3_table$objNum),
                           objLocTargetRating = c(schemaVR2_table$objLocTargetRating,
                                                  schemaVR3_table$objLocTargetRating),
                           AFCRec_per = c(schemaVR2_table$AFCRec_per,
                                      schemaVR3_table$AFCRec_per),
                           experiment = c(rep('2', length(schemaVR2_table$objNum)),
                                          rep('3', length(schemaVR3_table$objNum))))

# Transforming so that data is no bound between 0 and 1
combinedData$asinPR <- asin((combinedData$AFCRec_per*2)-1)
combinedData$asinPR <- scale(combinedData$asinPR)


# /* 
# ----------------------------- Model ---------------------------
# */
combinedData_priors <- c(prior(normal(0, 1), class = "Intercept"),
                         prior(normal(0, 1), class = "b")) 


combinedData_rec_AFC  <- brm(asinPR ~ objLocTargetRating +
                              (1 | objNum),
                          data = combinedData,
                          cores = cores2use,
                          save_all_pars = TRUE,
                          sample_prior = TRUE,
                          prior = combinedData_priors,
                          seed = seed,
                          control = list(adapt_delta = 0.99999, max_treedepth = 20))


# /* 
# ----------------------------- Saving iamge ---------------------------
# */
save.image(datedFileNam('bayesianMM_recollection', '.RData'))


# Logistic
schemaVR2_remembered <- rep(0, dim(dataSchemaVR2)[1])
schemaVR2_remembered[dataSchemaVR2$resCon == 1] <- 1

schemaVR3_remembered <- rep(0, dim(dataSchemaVR3)[1])
schemaVR3_remembered[dataSchemaVR3$resCon == 1] <- 1

schemaVR2_familiar <- rep(0, dim(dataSchemaVR2)[1])
schemaVR2_familiar[dataSchemaVR2$resCon == 2] <- 1

schemaVR3_familiar <- rep(0, dim(dataSchemaVR3)[1])
schemaVR3_familiar[dataSchemaVR3$resCon == 2] <- 1

combinedData <- data.frame(objNum = c(dataSchemaVR2$objNum,
                                      dataSchemaVR3$objNum),
                           subNum = c(dataSchemaVR2$subNum + 1,
                                      dataSchemaVR3$subNum),
                           accAFC = c(dataSchemaVR2$accAFC,
                                      dataSchemaVR3$accAFC),
                           objLocTargetRating = c(dataSchemaVR2$objLocTargetRating,
                                                  dataSchemaVR3$objLocTargetRating),
                           remembered = c(schemaVR2_remembered,
                                          schemaVR3_remembered),
                           familiar  =  c(schemaVR2_familiar,
                                          schemaVR3_familiar),
                           experiment = c(rep('2', length(dataSchemaVR2$objNum)),
                                          rep('3', length(dataSchemaVR3$objNum))))
combinedData$objLocTargetRating <- scale(combinedData$objLocTargetRating)

combinedData_priors <- c(prior(normal(0, 1), class = "Intercept"),
                         prior(normal(0, 1), class = "b")) 

combinedData_logistic_rem <- brm(remembered ~ objLocTargetRating +  I(objLocTargetRating*objLocTargetRating)+
                                    (1 | objNum) +
                                    (1 | subNum),
                                  data = subset(combinedData, accAFC == 1),
                                  cores = cores2use,
                                  family = bernoulli(),
                                  save_all_pars = TRUE,
                                  sample_prior = TRUE,
                                  prior = combinedData_priors,
                                  seed = seed,
                                  control = list(adapt_delta = 0.99999, max_treedepth = 20))


combinedData_logistic_fam  <- brm(familiar ~ objLocTargetRating +  I(objLocTargetRating*objLocTargetRating)+
                               (1 | objNum) +
                               (1 | subNum),
                             data = subset(combinedData, accAFC == 1),
                             cores = cores2use,
                             family = bernoulli(),
                             save_all_pars = TRUE,
                             sample_prior = TRUE,
                             prior = combinedData_priors,
                             seed = seed,
                             control = list(adapt_delta = 0.99999, max_treedepth = 20))

plot(marginal_effects(combinedData_logistic_rem), points = TRUE, rug = TRUE)
plot(marginal_effects(combinedData_logistic_fam), points = TRUE, rug = TRUE)
library(ggplot2)



ggplot(subset(combinedData, accAFC == 1), aes(y = remembered, x = objLocTargetRating))+ geom_point() + geom_smooth()

dataSchemaVR2$schemaVR2_remembered <- schemaVR2_remembered

#dataSchemaVR2$objLocTargetRating <- scale(objLocTargetRating)
agg <- ddply(dataSchemaVR2, c('objNum'), summarise, exp = mean(objLocTargetNorm), rem = mean(schemaVR2_remembered))
ggplot(agg, aes(x = exp, y = rem)) + geom_point() + geom_smooth()


schemaVR2_familiar <- rep(0, dim(dataSchemaVR2)[1])
schemaVR2_familiar[dataSchemaVR2$resCon == 2] <- 1
#schemaVR2_familiar[dataSchemaVR2$resCon == 1] <- 1
dataSchemaVR2$schemaVR2_familiar <- schemaVR2_familiar
ggplot(subset(dataSchemaVR2, schemaVR2_remembered == 0), aes(x = objLocTargetRating, y = schemaVR2_familiar)) + geom_jitter(height = 0.1) + geom_smooth()
