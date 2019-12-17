# Script to run a two lines test across all experiments
# Version 1.0
# Date: 21/10/2019
# Author: Joern Alexander Quent
# /*
# ----------------------------- Libraries, settings and functions ---------------------------
# */
# Setting seed and cores
set.seed(39257)
cores2use <- 4

# Libraries
library(brms)
library(assortedRFunctions)

# Loading data
load("U:/Projects/schemaVR/schemaVR1/data/dataSchemaVR1_cleaned.RData")
load("U:/Projects/schemaVR/schemaVR2/data/dataSchemaVR2_cleaned.RData")
load("U:/Projects/schemaVR/schemaVR3/data/dataSchemaVR3_cleaned.RData")

# Recall data
# Combine data to one data frame
combinedData_recall <- data.frame(Experiment = c(rep('1', length(dataSchemaVR1_recall$subNum)),
                                          rep('2', length(dataSchemaVR2_recall$subNum)),
                                          rep('3', length(dataSchemaVR3_recall$subNum))),
                           subNum = c(dataSchemaVR1_recall$subNum,
                                      dataSchemaVR2_recall$subNum,
                                      dataSchemaVR3_recall$subNum),
                           objNum = c(dataSchemaVR1_recall$objNum,
                                      dataSchemaVR2_recall$objNum,
                                      dataSchemaVR3_recall$objNum),
                           objLocTargetRating = c(dataSchemaVR1_recall$objLocTargetRating,
                                                  dataSchemaVR2_recall$objLocTargetRating,
                                                  dataSchemaVR3_recall$objLocTargetRating),
                           accRecall = c(dataSchemaVR1_recall$accRecall,
                                         dataSchemaVR2_recall$accRecall,
                                         dataSchemaVR3_recall$accRecall),
                           euclideanDist = c(dataSchemaVR1_recall$euclideanDist,
                                             dataSchemaVR2_recall$euclideanDist,
                                             dataSchemaVR3_recall$euclideanDist))
combinedData_recall$Exp  <- combinedData_recall$objLocTargetRating 

# 3AFC
# Combine data to one data frame
combinedData_AFC <- data.frame(Experiment = c(rep('1', length(dataSchemaVR1_AFC$subNum)),
                                                 rep('2', length(dataSchemaVR2_AFC$subNum)),
                                                 rep('3', length(dataSchemaVR3_AFC$subNum))),
                                  subNum = c(dataSchemaVR1_AFC$subNum,
                                             dataSchemaVR2_AFC$subNum,
                                             dataSchemaVR3_AFC$subNum),
                                  objNum = c(dataSchemaVR1_AFC$objNum,
                                             dataSchemaVR2_AFC$objNum,
                                             dataSchemaVR3_AFC$objNum),
                                  objLocTargetRating = c(dataSchemaVR1_AFC$objLocTargetRating,
                                                         dataSchemaVR2_AFC$objLocTargetRating,
                                                         dataSchemaVR3_AFC$objLocTargetRating),
                                  accAFC = c(dataSchemaVR1_AFC$accAFC,
                                                dataSchemaVR2_AFC$accAFC,
                                                dataSchemaVR3_AFC$accAFC))
combinedData_AFC$Exp  <- combinedData_AFC$objLocTargetRating 

# /*
# ----------------------------- Recall  model ---------------------------
# */
# Scaling based on Gelman et al. (2008) and https://github.com/stan-dev/stan/wiki/Prior-Choice-Recommendations
# Mean = 0 and SD = 0.5
combinedData_recall$sExp <- (combinedData_recall$Exp - mean(combinedData_recall$Exp ))/(sd(combinedData_recall$Exp)/0.5)

# Simulation parameters
br_recall      <- seq(-0.5, 0.5, 0.05)
nSim           <- length(br_recall)
results_recall <- list()
seeds          <- sample(9999999, nSim)

# Priors for all runs
prior_twolines_recall  <- c(prior(student_t(3, 0, 10) , class = "Intercept"),
                            prior(student_t(3, 0, 10) , class = "b", coef = 'high'),
                            prior(student_t(3, 0, 2.5) , class = "b", coef = 'xlow'),
                            prior(student_t(3, 0, 2.5)  , class = "b", coef = 'xhigh')) 

# Extract x-values for all runs
x <- combinedData_recall$sExp 

# Run 1 for model compilation
combinedData_recall$xlow  <- ifelse(x <= br_recall[1], x - br_recall[1], 0)
combinedData_recall$xhigh <- ifelse(x > br_recall[1], x - br_recall[1], 0)     
combinedData_recall$high  <- ifelse(x > br_recall[1], 1, 0)


model_twolines_recall <- brm(accRecall ~  xlow + xhigh + high +
                               (1 | subNum) +
                               (1 | objNum),
                             data = combinedData_recall,
                             prior = prior_twolines_recall,
                             family = bernoulli(),
                             cores = cores2use,
                             seed = seeds[1],
                             save_all_pars = TRUE,
                             sample_prior = TRUE)

results_recall[[1]] <- model_twolines_recall

for(i in 2:(nSim)){
  combinedData_recall$xlow  <- ifelse(x <= br_recall[i], x - br_recall[i], 0)
  combinedData_recall$xhigh <- ifelse(x > br_recall[i], x - br_recall[i], 0)     
  combinedData_recall$high  <- ifelse(x > br_recall[i], 1, 0)
  
  
  results_recall[[i]] <- update(model_twolines_recall,
                                newdata = combinedData_recall,
                                recompile = FALSE,
                                cores = cores2use,
                                save_all_pars = TRUE,
                                sample_prior = TRUE,
                                seed = seeds[i])
  
}

# /*
# ----------------------------- Euclidean distance  model ---------------------------
# */
# Scaling 
combinedData_euclid      <- combinedData_recall
combinedData_euclid$sExp <- scale(combinedData_euclid$objLocTargetRating)

# Simulation parameters
br_euclid      <- seq(-0.8, 0.8, 0.1)
nSim           <- length(br_euclid)
results_euclid <- list()
seeds          <- sample(9999999, nSim)

# Priors for all runs
prior_twolines_euclid  <- c(prior(normal(0, 1) , class = "Intercept"),
                            prior(normal(0, 1) , class = "b", coef = 'high'),
                            prior(normal(0, 1) , class = "b", coef = 'xlow'),
                            prior(normal(0, 1)  , class = "b", coef = 'xhigh')) 

# Extract x-values for all runs
x <- combinedData_euclid$sExp 

# Run 1 for model compilation
combinedData_euclid$xlow  <- ifelse(x <= br_euclid[1], x - br_euclid[1], 0)
combinedData_euclid$xhigh <- ifelse(x > br_euclid[1], x - br_euclid[1], 0)     
combinedData_euclid$high  <- ifelse(x > br_euclid[1], 1, 0)


model_twolines_euclid <- brm(euclideanDist ~  xlow + xhigh + high +
                               (1 | subNum) +
                               (1 | objNum),
                             data = combinedData_euclid,
                             prior = prior_twolines_euclid,
                             family = Gamma(link = "log"),
                             cores = 4,
                             save_all_pars = TRUE,
                             sample_prior = TRUE,
                             seed = seeds[1])

results_euclid[[1]] <- model_twolines_euclid

for(i in 2:(nSim)){
  combinedData_euclid$xlow  <- ifelse(x <= br_euclid[i], x - br_euclid[i], 0)
  combinedData_euclid$xhigh <- ifelse(x > br_euclid[i], x - br_euclid[i], 0)     
  combinedData_euclid$high  <- ifelse(x > br_euclid[i], 1, 0)
  
  results_euclid[[i]] <- update(model_twolines_euclid,
                                   newdata = combinedData_euclid,
                                   recompile = FALSE,
                                   cores = cores2use,
                                   save_all_pars = TRUE,
                                   sample_prior = TRUE,
                                   seed = seeds[i])
  
}

# /*
# ----------------------------- 3AFC model ---------------------------
# */
# Scaling based on Gelman et al. (2008) and https://github.com/stan-dev/stan/wiki/Prior-Choice-Recommendations
# Mean = 0 and SD = 0.5
combinedData_AFC$sExp <- (combinedData_AFC$Exp - mean(combinedData_AFC$Exp ))/(sd(combinedData_AFC$Exp)/0.5)

# Simulation parameters
br_AFC      <- seq(-0.5, 0.5, 0.05)
nSim           <- length(br_AFC)
results_AFC <- list()
seeds          <- sample(9999999, nSim)

# Priors for all runs
prior_twolines_AFC  <- c(prior(student_t(3, 0, 10) , class = "Intercept"),
                            prior(student_t(3, 0, 10) , class = "b", coef = 'high'),
                            prior(student_t(3, 0, 2.5) , class = "b", coef = 'xlow'),
                            prior(student_t(3, 0, 2.5)  , class = "b", coef = 'xhigh')) 

# Extract x-values for all runs
x <- combinedData_AFC$sExp 


# Run 1 for model compilation
combinedData_AFC$xlow  <- ifelse(x <= br_AFC[1], x - br_AFC[1], 0)
combinedData_AFC$xhigh <- ifelse(x > br_AFC[1], x - br_AFC[1], 0)     
combinedData_AFC$high  <- ifelse(x > br_AFC[1], 1, 0)


model_twolines_AFC <- brm(accAFC ~  xlow + xhigh + high +
                               (1 | subNum) +
                               (1 | objNum),
                             data = combinedData_AFC,
                             prior = prior_twolines_AFC,
                             family = bernoulli(),
                             cores = cores2use,
                             seed = seeds[1],
                             save_all_pars = TRUE,
                             sample_prior = TRUE)

results_AFC[[1]] <- model_twolines_AFC

for(i in 2:(nSim)){
  combinedData_AFC$xlow  <- ifelse(x <= br_AFC[i], x - br_AFC[i], 0)
  combinedData_AFC$xhigh <- ifelse(x > br_AFC[i], x - br_AFC[i], 0)     
  combinedData_AFC$high  <- ifelse(x > br_AFC[i], 1, 0)
  
  
  results_AFC[[i]] <- update(model_twolines_AFC,
                                newdata = combinedData_AFC,
                                recompile = FALSE,
                                cores = cores2use,
                                save_all_pars = TRUE,
                                sample_prior = TRUE,
                                seed = seeds[i])
}

# /*
# ----------------------------- Saving data ---------------------------
# */
save.image(datedFileNam('twoLineTests', '.RData'))