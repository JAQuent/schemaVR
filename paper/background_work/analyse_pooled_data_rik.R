# Run overall model
# Date: 22/03/2020
# Explanation: It's in the title :)
# Ask to remove everything in the global environment

#devtools::install_github("JAQuent/assortedRFunctions", upgrade = 'never')
library(assortedRFunctions)

assortedRFunctions::clear_environment()

# Setting seed
seed <- 1244
set.seed(seed)

# Setting WD
setwd("C:/Users/rh01/Desktop")

# /*
# ----------------------------- Libraries --------------------------
# */
library(brms)
library(rslurm)
library(polspline)


cores2use <- 4

# /*
# ----------------------------- Data --------------------------
# */
# Loading all .RData files
load("dataSchemaVR1_cleaned.RData")
load("dataSchemaVR2_cleaned.RData")
load("dataSchemaVR3_cleaned.RData")
load("dataSchemaVR4_cleaned.RData")

# Create AFC data for schemaVR4
dataSchemaVR4_AFC    <- subset(dataSchemaVR4, dataSchemaVR4$resCon != 0)

# Combine data to one data frame
combinedData_AFC <- data.frame(Experiment = c(rep('1', length(dataSchemaVR1_AFC$subNum)),
#                                              rep('2', length(dataSchemaVR2_AFC$subNum)),
#                                              rep('3', length(dataSchemaVR3_AFC$subNum)),
                                              rep('4', length(dataSchemaVR4_AFC$subNum))),
                               set        = c(rep('1', length(dataSchemaVR1_AFC$subNum)),
#                                              rep('2', length(dataSchemaVR2_AFC$subNum)),
#                                              as.character(dataSchemaVR3_AFC$setNum),
                                              as.character(dataSchemaVR4_AFC$setNum)),
                               subNum = c(as.character(dataSchemaVR1_AFC$subNum),
#                                          as.character(dataSchemaVR2_AFC$subNum),
#                                          as.character(dataSchemaVR3_AFC$subNum),
                                          as.character(dataSchemaVR4_AFC$subNum)),
                               objNum = c(dataSchemaVR1_AFC$objNum,
#                                          dataSchemaVR2_AFC$objNum,
#                                          dataSchemaVR3_AFC$objNum,
                                          dataSchemaVR4_AFC$objNum),
                               objLocTargetRating = c(dataSchemaVR1_AFC$objLocTargetRating,
#                                                      dataSchemaVR2_AFC$objLocTargetRating,
#                                                      dataSchemaVR3_AFC$objLocTargetRating,
                                                      dataSchemaVR4_AFC$objLocTargetRating),
                               accAFC = c(dataSchemaVR1_AFC$accAFC,
#                                          dataSchemaVR2_AFC$accAFC,
#                                          dataSchemaVR3_AFC$accAFC,
                                          dataSchemaVR4_AFC$accAFC))

combinedData_AFC$Exp    <- combinedData_AFC$objLocTargetRating 
combinedData_AFC$subNum <- as.character(combinedData_AFC$subNum)

# Scaling based on Gelman et al. (2008) and https://github.com/stan-dev/stan/wiki/Prior-Choice-Recommendations
# Mean = 0 and SD = 0.5
combinedData_AFC$sExp <- combinedData_AFC$Exp/(sd(combinedData_AFC$Exp)/0.5)
combinedData_AFC$y    <- combinedData_AFC$accAFC

Exp1Data_AFC <- subset(combinedData_AFC, Experiment == 1)
Exp4Data_AFC <- subset(combinedData_AFC, Experiment == 4)

# /*
# ----------------------------- Model --------------------------
# */
prior_schemaVR  <- c(prior(student_t(7, 0, 10) , class = "Intercept"),
                      prior(student_t(7, 0, 1) , class = "b")) 

model_Exp1_AFC <- brm(accAFC ~ sExp +  
                          I(sExp*sExp) +
                          (1 | subNum) +
                          (1| objNum),
                        data = Exp1Data_AFC,
                        prior = prior_schemaVR,
                        family = bernoulli(),
                        chains = 8,
                        warmup = 2000,
                        iter   = 16000,
                        cores = cores2use,
                        save_all_pars = TRUE,
                        sample_prior = TRUE,
                        seed = seed,
                        control = list(adapt_delta = 0.9)) 

# /*
# ----------------------------- BF --------------------------
# */
postDist     <- posterior_samples(model_Exp1_AFC)$b_IsExpMUsExp
areaPrior    <- integrate(dstudent_t, 
                          mu = 0,
                          df = 7, 
                          sigma = 1, 
                          lower = 0, 
                          upper = Inf, 
                          abs.tol = 0)$value 
priorDensity        <- dstudent_t(0 , 7, 0, 1)
bf_AFC_quad_Exp1  <- round(savage_dickey_ratio(postDist, priorDensity, areaPrior, 'greater'), 2)
bf_AFC_quad_Exp1


samples_for_priors <- 20000
postDists <- posterior_samples(model_Exp1_AFC)

intercept_Exp1_AFC   <- brm(b_Intercept ~ 1,
                                 data = postDists,
                                 cores = cores2use,
                                 family = student(link = "identity", link_sigma = "log", link_nu = "logm1"))

# Check, beep and sleep for 10 sec
pp_check(intercept_Exp1_AFC)

b_sExp_Exp1_AFC <- brm(b_sExp ~ 1,
                            data = postDists,
                            cores = cores2use,
                            family = student(link = "identity", link_sigma = "log", link_nu = "logm1"))

# Check, beep and sleep for 10 sec
pp_check(b_sExp_Exp1_AFC)


b_IsExpMUsExp_Exp1_AFC <- brm(b_IsExpMUsExp ~ 1,
                                   data = postDists,
                                   cores = cores2use,
                                   family = student(link = "identity", link_sigma = "log", link_nu = "logm1"))

# Check, beep and sleep for 10 sec
pp_check(b_IsExpMUsExp_Exp1_AFC)


prior_Exp4  <- c(set_prior(priorString_student(intercept_Exp1_AFC), 
                                class = "Intercept"),
                      set_prior(priorString_student(b_sExp_Exp1_AFC), 
                                class = "b", 
                                coef = "sExp"),
                      set_prior(priorString_student(b_IsExpMUsExp_Exp1_AFC), 
                                class = "b", 
                                coef = "IsExpMUsExp"))

model_Exp4_AFC <- brm(accAFC ~ sExp +  
                        I(sExp*sExp) +
                        (1 | subNum) +
                        (1| objNum),
                      data = Exp4Data_AFC,
                      prior = prior_Exp4,
                      family = bernoulli(),
                      chains = 8,
                      warmup = 2000,
                      iter   = 16000,
                      cores = cores2use,
                      save_all_pars = TRUE,
                      sample_prior = TRUE,
                      seed = seed,
                      control = list(adapt_delta = 0.9)) 


# /*
# ----------------------------- BF --------------------------
# */
postDist     <- posterior_samples(model_Exp4_AFC)$b_IsExpMUsExp
areaPrior    <- integrate(dstudent_t, 
                          mu = 0,
                          df = 7, 
                          sigma = 1, 
                          lower = 0, 
                          upper = Inf, 
                          abs.tol = 0)$value 
priorDensity        <- dstudent_t(0 , 7, 0, 1)
bf_AFC_quad_Exp1Exp4  <- round(savage_dickey_ratio(postDist, priorDensity, areaPrior, 'greater'), 2)
bf_AFC_quad_Exp1Exp4

#############################################

model_pooled_AFC <- brm(accAFC ~ sExp +  
                              I(sExp*sExp) +
                              (1 | subNum) +
                              (1| objNum),
                            data = combinedData_AFC,
                            prior = prior_schemaVR,
                            family = bernoulli(),
                            chains = 8,
                            warmup = 2000,
                            iter   = 16000,
                            cores = cores2use,
                            save_all_pars = TRUE,
                            sample_prior = TRUE,
                            seed = seed,
                            control = list(adapt_delta = 0.9)) 
# Summary and beep
summary(model_pooled_AFC)

# /*
# ----------------------------- BF --------------------------
# */
postDist     <- posterior_samples(model_pooled_AFC)$b_IsExpMUsExp
areaPrior    <- integrate(dstudent_t, 
                          mu = 0,
                          df = 7, 
                          sigma = 1, 
                          lower = 0, 
                          upper = Inf, 
                          abs.tol = 0)$value 
priorDensity        <- dstudent_t(0 , 7, 0, 1)
bf_AFC_quad_pooled  <- round(savage_dickey_ratio(postDist, priorDensity, areaPrior, 'greater'), 2)
bf_AFC_quad_pooled