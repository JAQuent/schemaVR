# Script to run a two line test across all experiments
# Version 1.0
# Date: 21/10/2019
# Author: Joern Alexander Quent
# /*
# ----------------------------- Libraries, settings and functions ---------------------------
# */
library(brms)
library(assortedRFunctions)

# Loading and preparing data
load("U:/Projects/schemaVR/schemaVR1/data/dataSchemaVR1_cleaned.RData")
load("U:/Projects/schemaVR/schemaVR2/data/dataSchemaVR2_cleaned.RData")
load("U:/Projects/schemaVR/schemaVR3/data/dataSchemaVR3_cleaned.RData")

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
# Scaling based on Gelman et al. (2008) and https://github.com/stan-dev/stan/wiki/Prior-Choice-Recommendations
# Mean = 0 and SD = 0.5
combinedData_recall$Exp  <- combinedData_recall$objLocTargetRating 

# /*
# ----------------------------- Recall  model ---------------------------
# */
# Loading data from schemaVR3
load("U:/Projects/schemaVR/schemaVR3/analysis/schemaVR3_recall_20191016_144708.RData")
recall_fixef <- fixef(model_schemaVR3_recall)
# Calculate breaking point by finding minimum of quadratic function
br_recall           <- quadMin(recall_fixef[3,1], recall_fixef[2,1]) 
br_recall           <- 0.45

# Creating variables for interrupted regression
# Fitting only one model for both lines because it doesn't make a difference for this data set.
dataSchemaVR1_recall$Exp  <- dataSchemaVR1_recall$objLocTargetRating 
dataSchemaVR1_recall$sExp <- (dataSchemaVR1_recall$Exp - mean(dataSchemaVR1_recall$Exp ))/(sd(dataSchemaVR1_recall$Exp)/0.5)
x <- dataSchemaVR1_recall$sExp 
dataSchemaVR1_recall$xlow  <- ifelse(x <= br_recall, x - br_recall, 0)
dataSchemaVR1_recall$xhigh <- ifelse(x > br_recall, x - br_recall, 0)     
dataSchemaVR1_recall$high  <- ifelse(x > br_recall, 1, 0)


prior_twolines_recall  <- c(prior(student_t(3, 0, 10) , class = "Intercept"),
                            prior(student_t(3, 0, 10) , class = "b", coef = 'high'),
                            prior(student_t(3, 0, 2.5) , class = "b", coef = 'xlow'),
                            prior(student_t(3, 0, 2.5)  , class = "b", coef = 'xhigh')) 


model_twolines_recall_schemaVR1 <- brm(accRecall ~  xlow + xhigh + high +
                               (1 | subNum) +
                               (1 | objNum),
                           data = dataSchemaVR1_recall,
                           prior = prior_twolines_recall,
                           cores = 4,
                           family = bernoulli(),
                           save_all_pars = TRUE,
                           sample_prior = TRUE)

br_recall           <- 0

# Creating variables for interrupted regression
# Fitting only one model for both lines because it doesn't make a difference for this data set.
dataSchemaVR2_recall$Exp  <- dataSchemaVR2_recall$objLocTargetRating 
dataSchemaVR2_recall$sExp <- (dataSchemaVR2_recall$Exp - mean(dataSchemaVR2_recall$Exp ))/(sd(dataSchemaVR2_recall$Exp)/0.5)
x <- dataSchemaVR2_recall$sExp 
dataSchemaVR2_recall$xlow  <- ifelse(x <= br_recall, x - br_recall, 0)
dataSchemaVR2_recall$xhigh <- ifelse(x > br_recall, x - br_recall, 0)     
dataSchemaVR2_recall$high  <- ifelse(x > br_recall, 1, 0)

model_twolines_recall_schemaVR2 <- brm(accRecall ~  xlow + xhigh + high +
                                         (1 | subNum) +
                                         (1 | objNum),
                                       data = dataSchemaVR2_recall,
                                       prior = prior_twolines_recall,
                                       cores = 4,
                                       family = bernoulli(),
                                       save_all_pars = TRUE,
                                       sample_prior = TRUE)


br_recall           <- -0.73

# Creating variables for interrupted regression
# Fitting only one model for both lines because it doesn't make a difference for this data set.
dataSchemaVR3_recall$Exp  <- dataSchemaVR3_recall$objLocTargetRating 
dataSchemaVR3_recall$sExp <- (dataSchemaVR3_recall$Exp - mean(dataSchemaVR3_recall$Exp ))/(sd(dataSchemaVR3_recall$Exp)/0.5)
x <- dataSchemaVR3_recall$sExp 
dataSchemaVR3_recall$xlow  <- ifelse(x <= br_recall, x - br_recall, 0)
dataSchemaVR3_recall$xhigh <- ifelse(x > br_recall, x - br_recall, 0)     
dataSchemaVR3_recall$high  <- ifelse(x > br_recall, 1, 0)

model_twolines_recall_schemaVR3 <- brm(accRecall ~  xlow + xhigh + high +
                                         (1 | subNum) +
                                         (1 | objNum),
                                       data = dataSchemaVR3_recall,
                                       prior = prior_twolines_recall,
                                       cores = 4,
                                       family = bernoulli(),
                                       save_all_pars = TRUE,
                                       sample_prior = TRUE)


br_recall <- seq(-0.5, 0.5, 0.05)
nSim      <- length(br_euclid)
results   <- list()
seeds     <- sample(9999999, nSim)

combinedData_recall$Exp  <- combinedData_recall$objLocTargetRating 
combinedData_recall$sExp <- (combinedData_recall$Exp - mean(combinedData_recall$Exp ))/(sd(combinedData_recall$Exp)/0.5)
x                        <- combinedData_recall$sExp 
prior_twolines_recall  <- c(prior(student_t(3, 0, 10) , class = "Intercept"),
                            prior(student_t(3, 0, 10) , class = "b", coef = 'high'),
                            prior(student_t(3, 0, 2.5) , class = "b", coef = 'xlow'),
                            prior(student_t(3, 0, 2.5)  , class = "b", coef = 'xhigh')) 


combinedData_recall$xlow  <- ifelse(x <= br_recall[1], x - br_recall[1], 0)
combinedData_recall$xhigh <- ifelse(x > br_recall[1], x - br_recall[1], 0)     
combinedData_recall$high  <- ifelse(x > br_recall[1], 1, 0)


model_twolines_recall <- brm(accRecall ~  xlow + xhigh + high +
                               (1 | subNum) +
                               (1 | objNum),
                             data = combinedData_recall,
                             prior = prior_twolines_recall,
                             cores = 4,
                             family = bernoulli(),
                             save_all_pars = TRUE,
                             sample_prior = TRUE)

results[[1]] <- model_twolines_recall

for(i in 1:(nSim - 1)){
  combinedData_recall$xlow  <- ifelse(x <= br_recall[i], x - br_recall[i], 0)
  combinedData_recall$xhigh <- ifelse(x > br_recall[i], x - br_recall[i], 0)     
  combinedData_recall$high  <- ifelse(x > br_recall[i], 1, 0)
  
  
  results[[i]] <- update(model_twolines_recall,
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
br_euclid <- seq(-0.8, 0.8, 0.1)
nSim      <- length(br_euclid)
results   <- list()
seeds     <- sample(9999999, nSim)

combinedData_euclid      <- combinedData_recall
combinedData_euclid$sExp <- scale(combinedData_euclid$objLocTargetRating)

x <- combinedData_euclid$sExp 
prior_twolines_euclid  <- c(prior(normal(0, 1) , class = "Intercept"),
                            prior(normal(0, 1) , class = "b", coef = 'high'),
                            prior(normal(0, 1) , class = "b", coef = 'xlow'),
                            prior(normal(0, 1)  , class = "b", coef = 'xhigh')) 



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
                             sample_prior = TRUE)
results[[1]] <- model_twolines_euclid

for(i in 1:(nSim - 1)){
  combinedData_euclid$xlow  <- ifelse(x <= br_euclid[i], x - br_euclid[i], 0)
  combinedData_euclid$xhigh <- ifelse(x > br_euclid[i], x - br_euclid[i], 0)     
  combinedData_euclid$high  <- ifelse(x > br_euclid[i], 1, 0)
  
  results[[i]] <- update(model_twolines_euclid,
                                   newdata = combinedData_euclid,
                                   recompile = FALSE,
                                   cores = cores2use,
                                   save_all_pars = TRUE,
                                   sample_prior = TRUE,
                                   seed = seeds[i])
  
}