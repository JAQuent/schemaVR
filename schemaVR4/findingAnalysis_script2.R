# Script for finding the right analysis. This analysis is tested with simulated data
# Version 1.2
# Date: 08/09/2019
# Author: Joern Alexander Quent
# Libraries
library(brms)
library(assortedRFunctions)
library(lmerTest)

# General variables
numNewSub  <- 36*2 # Data from time participants see the room first
uShapeSeed <- 2332
rkSeed     <- 253324

# /* 
# ----------------------------- U-Shape (Quadratic) ---------------------------
# */
# Load data
load("U:/Projects/schemaVR/schemaVR4/bayesianMM_recall_20190724_170733.RData")

# /* 
# ----------------------------- Case 1 (Results again like schemaVR2) ---------------------------
# */
# Seed
set.seed(uShapeSeed) # Use the same seed for both cases to have the same error structure

# Get parameters from schemaVR2
schemaVR2_beta <- summary(schemaVR2_recall_lmer)$coefficients

# General values and variables need for the for loop.
numObj <- 20
numSub <- numNewSub
e      <- rnorm(numObj * numSub, mean = 0, sd = 1)
x      <- scale(runif(numObj * numSub, min = -100, max = 100))
z      <- c()
index  <- 1

# Coefficients
gamma00 <- 0
gamma01 <- schemaVR2_beta[1,1]
gamma02 <- schemaVR2_beta[1,1]
beta1   <- schemaVR2_beta[2,1]
beta2   <- schemaVR2_beta[3,1]
w1      <- runif(numObj, min = 0, max = 3)
w2      <- runif(numSub, min = -3, max = 3)
u01     <- rnorm(numObj, mean = 0, sd = 0.1)
u02     <- rnorm(numSub, mean = 0, sd = 0.1)

# Generating dataset
for(j in 1:numSub){
  for(i in 1:numObj){
    z[index] <- gamma00 + gamma01*w1[i] + u01[i] + gamma02*w2[j] + u02[j] + beta1*x[index] + beta2*x[index]*x[index] + e[index]
    index <- index + 1
  } 
}

beta0_avg  <- gamma00 + mean(gamma01*w1) + mean(u01) + mean(gamma02*w2) + mean(u02)
pr         <-  1/(1+exp(-z))
y          <- rbinom(numObj*numSub, 1, pr)

# Binding to data frame
recall_case1_data <- data.frame(accRecall = y, 
                         objLocTargetRating = x, 
                         pr = pr,
                         subNum = factor(rep(1:numSub, each = numObj)), 
                         objNum = factor(rep(1:numObj, numSub)))


# Calculating lmer model
recall_case1_model_lmer <- glmer(accRecall ~  objLocTargetRating +
                            I(objLocTargetRating*objLocTargetRating) + 
                            (1 | subNum) +
                            (1 | objNum),
                          family = binomial,
                          data = recall_case1_data,
                          control = glmerControl(optimizer = "bobyqa"),
                          nAGQ = 1)

# Using results from schemaVR3 as priors
recall_case1_priors <- c(set_prior(priorString_normal(schemaVR3_recall_fixef[1, 1], schemaVR3_recall_fixef[1, 2]),
                            class = "Intercept"),
                  set_prior(priorString_normal(schemaVR3_recall_fixef[2, 1], schemaVR3_recall_fixef[2, 2]),
                            class = "b",
                            coef = "objLocTargetRating"),
                  set_prior(priorString_normal(schemaVR3_recall_fixef[3, 1], schemaVR3_recall_fixef[3, 2]),
                            class = "b",
                            coef = "IobjLocTargetRatingMUobjLocTargetRating"))

# Calculating BRMS model
recall_case1_model <- brm(accRecall ~ objLocTargetRating +  
                     I(objLocTargetRating*objLocTargetRating) +
                     (1 | subNum) +
                     (1 | objNum),
                   data = recall_case1_data,
                   cores = cores2use,
                   family = bernoulli(),
                   save_all_pars = TRUE,
                   sample_prior = TRUE,
                   prior = recall_case1_priors)

# Extracting results
recall_case1_postDen    <- posterior_samples(recall_case1_model)
recall_case1_fixef      <- round(fixef(recall_case1_model), 3)
recall_case1_fixef_lmer <- round(fixef(recall_case1_model_lmer), 3) 
recall_case1_ratio      <- hypothesis(recall_case1_model, "IobjLocTargetRatingMUobjLocTargetRating = 0")$`hypothesis`$Evid.Ratio

# /* 
# ----------------------------- Case 2 (True null) ---------------------------
# */
# Seed
set.seed(uShapeSeed) # Use the same seed for both cases to have the same error structure

# Get parameters from schemaVR3
schemaVR3_beta <- summary(schemaVR3_recall_lmer)$coefficients

# General values and variables
numObj <- 20
numSub <- numNewSub
e      <- rnorm(numObj * numSub, mean = 0, sd = 1)
x      <- scale(runif(numObj * numSub, min = -100, max = 100))
z      <- c()
index  <- 1

# Coefficients
gamma00 <- 0
gamma01 <- schemaVR3_beta[1,1]
gamma02 <- schemaVR3_beta[1,1]
beta1   <- schemaVR3_beta[2,1]
beta2   <- 0
w1      <- runif(numObj, min = 0, max = 3)
w2      <- runif(numSub, min = -3, max = 3)
u01     <- rnorm(numObj, mean = 0, sd = 0.1)
u02     <- rnorm(numSub, mean = 0, sd = 0.1)

# Generating dataset
for(j in 1:numSub){
  for(i in 1:numObj){
    z[index] <- gamma00 + gamma01*w1[i] + u01[i] + gamma02*w2[j] + u02[j] + beta1*x[index] + beta2*x[index]*x[index] + e[index]
    index <- index + 1
  } 
}

beta0_avg <- gamma00 + mean(gamma01*w1) + mean(u01) + mean(gamma02*w2) + mean(u02)
pr        <-  1/(1+exp(-z))
y         <- rbinom(numObj*numSub, 1, pr)

# Binding to data frame
recall_case2_data <- data.frame(accRecall = y, 
                         objLocTargetRating = x, 
                         pr = pr,
                         subNum = factor(rep(1:numSub, each = numObj)), 
                         objNum = factor(rep(1:numObj, numSub)))

# Calculating lmer model
recall_case2_model_lmer <- glmer(accRecall ~  objLocTargetRating +
                            I(objLocTargetRating*objLocTargetRating) + 
                            (1 | subNum) +
                            (1 | objNum),
                          family = binomial,
                          data = recall_case2_data,
                          control = glmerControl(optimizer = "bobyqa"),
                          nAGQ = 1)

# Using results from schemaVR3 as priors
recall_case2_priors <- c(set_prior(priorString_normal(schemaVR3_recall_fixef[1, 1], schemaVR3_recall_fixef[1, 2]),
                            class = "Intercept"),
                  set_prior(priorString_normal(schemaVR3_recall_fixef[2, 1], schemaVR3_recall_fixef[2, 2]),
                            class = "b",
                            coef = "objLocTargetRating"),
                  set_prior(priorString_normal(schemaVR3_recall_fixef[3, 1], schemaVR3_recall_fixef[3, 2]),
                            class = "b",
                            coef = "IobjLocTargetRatingMUobjLocTargetRating"))
# A possible difference could be that here the other parameters actually match up only the quadratic term doesn't.


# Calculating BRMS model
recall_case2_model <- brm(accRecall ~ objLocTargetRating +  
                     I(objLocTargetRating*objLocTargetRating) +
                     (1 | subNum) +
                     (1 | objNum),
                   data = recall_case2_data,
                   cores = cores2use,
                   family = bernoulli(),
                   save_all_pars = TRUE,
                   sample_prior = TRUE,
                   prior = recall_case2_priors)

# Extracting results
recall_case2_postDen    <- posterior_samples(recall_case2_model)
recall_case2_fixef      <- round(fixef(recall_case2_model), 3)
recall_case2_fixef_lmer <- round(fixef(recall_case2_model_lmer), 3) # Control that estimate
recall_case2_ratio      <- hypothesis(recall_case2_model, "IobjLocTargetRatingMUobjLocTargetRating = 0")$`hypothesis`$Evid.Ratio

# /* 
# ----------------------------- U-Shape (two line test) ---------------------------
# */
# Calculating minimum values of quadatric term for schemaVR1, schemaVR2,schemaVR3
minima <- data.frame(Experiment = c('schemaVR1', 
                                    'schemaVR2', 
                                    'schemaVR3'),
                     MinimumLmer = c(quadMin(schemaVR1_recall_fixef_lmer[3], schemaVR1_recall_fixef_lmer[2]),
                                 quadMin(schemaVR2_recall_fixef_lmer[3], schemaVR2_recall_fixef_lmer[2]),
                                 quadMin(schemaVR3_recall_fixef_lmer[3], schemaVR3_recall_fixef_lmer[2])),
                     MinimumBRMS = c(quadMin(schemaVR1_recall_fixef[3, 1], schemaVR1_recall_fixef[2, 1]),
                                     quadMin(schemaVR2_recall_fixef[3, 1], schemaVR1_recall_fixef[2, 1]),
                                     quadMin(schemaVR3_recall_fixef[3, 1], schemaVR1_recall_fixef[2, 1])))

meanMinimum <- mean(minima$MinimumLmer) # This will be the breaking point for the analyses


# /* 
# ----------------------------- Recollection ---------------------------
# */
# Load data
load("U:/Projects/schemaVR/schemaVR4/bayesianMM_RK_20190729_125333.RData")

# /* 
# ----------------------------- Case 1 (Results again like schemaVR3) ---------------------------
# */
# Seed
set.seed(rkSeed)

# Create data frame
rk_case1 <- data.frame(objNum = schemaVR3_table4$objNum, objLocTargetRating = schemaVR3_table4$objLocTargetRating)

# Generating data
beta0               <- summary(schemaVR3_rec_AFC_lmer)$coefficients[1]
beta1               <- summary(schemaVR3_rec_AFC_lmer)$coefficients[2, 1] - 0.05
errorSD             <- summary(schemaVR3_rec_AFC_lmer)$coefficients[2, 2]*10
rk_case1$AFCRec_per <- beta0 + beta1*rk_case1$objLocTargetRating + rnorm(dim(rk_case1)[1], 0, errorSD)

# Calculating lmer model
rk_case1_model_lmer <- lmer(AFCRec_per ~  objLocTargetRating  + 
                              (1 | objNum),
                            data = rk_case1)

# Using results from schemaVR3 as priors
rk_case1_priors <- c(set_prior(priorString_normal(schemaVR3_rec_AFC_fixef[1, 1], schemaVR3_rec_AFC_fixef[1, 2]),
                               class = "Intercept"),
                     set_prior(priorString_normal(schemaVR3_rec_AFC_fixef[2, 1], schemaVR3_rec_AFC_fixef[2, 2]),
                               class = "b",
                               coef = "objLocTargetRating"))

# Calculating BRMS model
rk_case1_model  <- brm(AFCRec_per ~ objLocTargetRating +
                         (1 | objNum) ,
                       data = rk_case1,
                       cores = cores2use,
                       save_all_pars = TRUE,
                       sample_prior = TRUE,
                       prior = rk_case1_priors,
                       control = list(adapt_delta = 0.95))

# Extracting results
rk_case1_postDen    <- posterior_samples(rk_case1_model)
rk_case1_fixef      <- round(fixef(rk_case1_model), 3)
rk_case1_fixef_lmer <- round(fixef(rk_case1_model_lmer), 3) 
rk_case1_ratio      <- hypothesis(rk_case1_model, "objLocTargetRating = 0")$`hypothesis`$Evid.Ratio

# /* 
# ----------------------------- Case 2 (true null)---------------------------
# */
# Seed
set.seed(rkSeed) # Use the same seed for both cases to have the same error structure

# Create data frame
rk_case2 <- data.frame(objNum = schemaVR3_table4$objNum, objLocTargetRating = schemaVR3_table4$objLocTargetRating)

# Generating data
beta0               <- summary(schemaVR3_rec_AFC_lmer)$coefficients[1]
beta1               <- 0.3
errorSD             <- 1
rk_case2$AFCRec_per <- beta0 + beta1*rk_case1$objLocTargetRating + rnorm(dim(rk_case2)[1], 0, errorSD)

# Calculating lmer model
rk_case2_model_lmer <- lmer(AFCRec_per ~  objLocTargetRating  + 
                              (1 | objNum),
                            data = rk_case2)

# Using results from schemaVR3 as priors
rk_case2_priors <- c(set_prior(priorString_normal(schemaVR3_rec_AFC_fixef[1, 1], schemaVR3_rec_AFC_fixef[1, 2]),
                               class = "Intercept"),
                     set_prior(priorString_normal(schemaVR3_rec_AFC_fixef[2, 1], schemaVR3_rec_AFC_fixef[2, 2]),
                               class = "b",
                               coef = "objLocTargetRating"))

# Calculating BRMS model
rk_case2_model  <- brm(AFCRec_per ~ objLocTargetRating +
                         (1 | objNum) ,
                       data = rk_case2,
                       cores = cores2use,
                       save_all_pars = TRUE,
                       sample_prior = TRUE,
                       prior = rk_case2_priors,
                       control = list(adapt_delta = 0.95))

# Extracting results
rk_case2_postDen    <- posterior_samples(rk_case2_model)
rk_case2_fixef      <- round(fixef(rk_case2_model), 3)
rk_case2_fixef_lmer <- round(fixef(rk_case2_model_lmer), 3) 
rk_case2_ratio      <- hypothesis(rk_case2_model, "objLocTargetRating = 0")$`hypothesis`$Evid.Ratio

# /* 
# ----------------------------- Model including all experiments ---------------------------
# */
# Concatenating data
combinedData <- data.frame(accRecall = c(dataSchemaVR1_recall$accRecall,
                                         dataSchemaVR2_recall$accRecall,
                                         dataSchemaVR3_recall$accRecall),
                           objLocTargetRating = c(dataSchemaVR1_recall$objLocTargetRating,
                                                  dataSchemaVR2_recall$objLocTargetRating,
                                                  dataSchemaVR3_recall$objLocTargetRating),
                           subNum = c(dataSchemaVR1_recall$subNum,
                                      dataSchemaVR2_recall$subNum,
                                      dataSchemaVR3_recall$subNum),
                           objNum = c(dataSchemaVR1_recall$objNum,
                                      dataSchemaVR2_recall$objNum,
                                      dataSchemaVR3_recall$objNum),
                           experiment = c(rep('1', length(dataSchemaVR1_recall$accRecall)),
                                          rep('2', length(dataSchemaVR2_recall$accRecall)),
                                          rep('3', length(dataSchemaVR3_recall$accRecall))))

# Choice of prior see here https://github.com/stan-dev/stan/wiki/Prior-Choice-Recommendations
# As suggested data is scale free -> weakly informative prior
combinedData_priors <- c(prior(normal(0, 1), class = "Intercept"),
                         prior(normal(0, 1), class = "b")) 


combinedData_model  <- brm(accRecall ~ objLocTargetRating +  
                         I(objLocTargetRating*objLocTargetRating) +
                           (1 | subNum) +
                           (1 | objNum),
                       data = combinedData,
                       family = bernoulli(),
                       prior = combinedData_priors,
                       cores = cores2use,
                       save_all_pars = TRUE,
                       sample_prior = TRUE,
                       iter = 4000, # Because no convergence with default
                       seed = 1298,
                       control = list(adapt_delta = 0.98)) # Increased because of divergent transitions


combinedData_model_null <- brm(accRecall ~ objLocTargetRating  +
                                 (objLocTargetRating | experiment) +
                                 (1 | subNum) +
                                 (1 | objNum),
                               data = combinedData,
                               family = bernoulli(),
                               prior = combinedData_priors,
                               cores = cores2use,
                               save_all_pars = TRUE,
                               sample_prior = TRUE,
                               iter = 4000, # Because no convergence with default
                               seed = 1298,
                               control = list(adapt_delta = 0.98)) # Increased because of divergent transitions



bayes_factor(combinedData_model, combinedData_model_null)


summary(combinedData_model)
hypothesis(combinedData_model, "IobjLocTargetRatingMUobjLocTargetRating > 0")
hypothesis(combinedData_model, "IobjLocTargetRatingMUobjLocTargetRating = 0")
combinedData_model_postDen    <- posterior_samples(combinedData_model)

# Add new simulated data 
combinedData_case1 <- data.frame(accRecall = c(combinedData$accRecall,recall_case1_data$accRecall),
                                 objLocTargetRating = c(combinedData$objLocTargetRating,recall_case1_data$objLocTargetRating),
                                 subNum = c(combinedData$subNum,recall_case1_data$subNum),
                                 objNum  =c(combinedData$objNum,recall_case1_data$objNum),
                                 experiment = c(combinedData$experiment, rep('4', length(recall_case1_data$accRecall))))

# Run model
combinedData_case1_model  <- brm(accRecall ~ objLocTargetRating +  
                             I(objLocTargetRating*objLocTargetRating) +
                             (objLocTargetRating + I(objLocTargetRating*objLocTargetRating) | experiment),
                           family = bernoulli(),
                           prior = combinedData_priors,
                           data = combinedData_case1,
                           cores = cores2use,
                           save_all_pars = TRUE,
                           sample_prior = TRUE,
                           iter = 2000, # Because no convergence with default
                           seed = 1298,
                           control = list(adapt_delta = 0.95)) # Increased because of divergent transitions

summary(combinedData_case1_model)
hypothesis(combinedData_case1_model, "IobjLocTargetRatingMUobjLocTargetRating = 0")
combinedData_case1_model_postDen    <- posterior_samples(combinedData_case1_model)
plot(hypothesis(combinedData_case1_model, "IobjLocTargetRatingMUobjLocTargetRating = 0"))
plot(marginal_effects(combinedData_case1_model), points = TRUE, rug = TRUE)
ranef(combinedData_case1_model)

# https://www.jamesrrae.com/post/bayesian-logistic-regression-using-brms-part-1/
# https://stats.stackexchange.com/questions/137663/how-to-specify-a-random-slope-only-in-glmer
plot(marginal_effects(schemaVR1_recall), points = TRUE, rug = TRUE)

# /* 
# ----------------------------- Saving data ---------------------------
# */
save.image(datedFileNam('findingAnalysis', '.RData'))