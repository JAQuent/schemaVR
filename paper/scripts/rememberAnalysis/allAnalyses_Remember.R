# Script that runs all analyses for the remember data
# Version 1.0
# Date: 05/12/2019
# Author: Joern Alexander Quent
# /* 
# ----------------------------- Libraries, settings and functions ---------------------------
# */
# Setting seed
set.seed(986739)

# Libaries
library(brms)
library(rslurm)

# /* 
# ----------------------------- Loading data ---------------------------
# */
# Loading data
load("/home/aq01/Projects/schemaVR/schemaVR2/data/dataSchemaVR2_cleaned.RData")
load('/home/aq01/Projects/schemaVR/schemaVR3/data/schemaVR3_closestLocation.RData')

# Make subNum character
dataSchemaVR2$subNum <- as.character(dataSchemaVR2$subNum)
dataSchemaVR3$subNum <- as.character(dataSchemaVR3$subNum)

# Create remember variable
remembered <- rep(0, dim(dataSchemaVR2)[1])
remembered[dataSchemaVR2$resCon == 1] <- 1
dataSchemaVR2$remembered <- remembered

# Create remember variable
remembered <- rep(0, dim(dataSchemaVR3)[1])
remembered[dataSchemaVR3$resCon == 1] <- 1
dataSchemaVR3$remembered <- remembered

# Combine datasets
combinedData <- data.frame(subNum = c(dataSchemaVR2$subNum, dataSchemaVR3$subNum),
                           objNum = c(dataSchemaVR2$objNum, dataSchemaVR3$objNum),
                           resCon = c(dataSchemaVR2$resCon, dataSchemaVR3$resCon),
                           objLocTargetNorm = c(dataSchemaVR2$objLocTargetNorm, dataSchemaVR3$objLocTargetNorm),
                           objLocTargetRating = c(dataSchemaVR2$objLocTargetRating, dataSchemaVR3$objLocTargetRating),
                           remembered = c(dataSchemaVR2$remembered, dataSchemaVR3$remembered),
                           accRecall = c(dataSchemaVR2$accRecall, dataSchemaVR3$accRecall))

# Scaling based on Gelman et al. (2008) and https://github.com/stan-dev/stan/wiki/Prior-Choice-Recommendations
# Mean = 0 and SD = 0.5
combinedData$Exp_ind  <- combinedData$objLocTargetRating 
combinedData$sExp_ind <- (combinedData$Exp_ind - mean(combinedData$Exp_ind))/(sd(combinedData$Exp_ind)/0.5)

combinedData$Exp_norm  <- combinedData$objLocTargetNorm 
combinedData$sExp_norm <- (combinedData$Exp_norm - mean(combinedData$Exp_norm))/(sd(combinedData$Exp_norm)/0.5)

# /* 
# ----------------------------- General parameters ---------------------------
# */
# BRMS parameters
chains       <- 1
nRuns        <- 8
iterPerChain <- 4000
seeds        <- sample(99999, nRuns)

# Job parameters
n_nodes       <- 1
cpus_per_node <- nRuns # 1 cpu per run

# Priors
priors <- c(prior(student_t(3, 0, 10) , class = "Intercept"),
            prior(student_t(3, 0, 2.5) , class = "b")) 

# /* 
# ----------------------------- Normative data (model 1) ---------------------------
# */
seeds <- sample(99999, nRuns)
pars  <- data.frame(i = 1:nRuns, seed = seeds)

# Run 1 for model compilation
model1 <- brm(remembered ~ sExp_norm +
                I(sExp_norm*sExp_norm) +
                (1 | subNum) +
                (1 | objNum),
              data = combinedData,
              prior = priors,
              family = bernoulli(),
              cores = 1,
              chains = 1,
              iter = iterPerChain,
              save_all_pars = TRUE,
              sample_prior = TRUE,
              save_dso = TRUE, 
              seed = pars[1, 'seed']) 

# rslurm function
model1_fun <- function(i, seed){
  if(i == 1){
    return(list(i = i,
                seed = seed,
                model = model1))
  } else {
    model1 <- update(model1,
                     newdata = combinedData,
                     recompile = FALSE,
                     cores = 1,
                     chains = 1,
                     iter = iterPerChain,
                     save_all_pars = TRUE,
                     sample_prior = TRUE,
                     save_dso = TRUE,
                     seed = seed)
    return(list(i = i,
                seed = seed,
                model = model1))
  }
  
}

# Create rslurm job 
sjob1 <- slurm_apply(model1_fun, pars, jobname = 'model1',
                     add_objects = c("model1", 'combinedData', 'iterPerChain'),
                     nodes = n_nodes, cpus_per_node = cpus_per_node, submit = FALSE)

# /* 
# ----------------------------- All trials (model 2) ---------------------------
# */
# Run 1 for model compilation
model2 <- brm(remembered ~ sExp_ind +
                I(sExp_ind*sExp_ind) +
                (1 | subNum) +
                (1 | objNum),
              data = combinedData,
              prior = priors,
              family = bernoulli(),
              cores = 1,
              chains = 1,
              iter = iterPerChain,
              save_all_pars = TRUE,
              sample_prior = TRUE,
              save_dso = TRUE, 
              seed = pars[1, 'seed']) 

# rslurm function
model2_fun <- function(i, seed){
  if(i == 1){
    return(list(i = i,
                seed = seed,
                model = model2))
  } else {
    model3 <- update(model2,
                     newdata = combinedData,
                     recompile = FALSE,
                     cores = 1,
                     chains = 1,
                     iter = iterPerChain,
                     save_all_pars = TRUE,
                     sample_prior = TRUE,
                     save_dso = TRUE,
                     seed = seed)
    return(list(i = i,
                seed = seed,
                model = model2))
  }
  
}

# Create rslurm job 
sjob1 <- slurm_apply(model2_fun, pars, jobname = 'model2',
                     add_objects = c("model2", 'combinedData', 'iterPerChain'),
                     nodes = n_nodes, cpus_per_node = cpus_per_node, submit = FALSE)


# /* 
# ----------------------------- Correct only (model 3) ---------------------------
# */
combinedData_sub1 <- combinedData[combinedData$accRecall == 1, ] # only correct trials

# rslurm function
model3_fun <- function(i, seed){
  model3 <- update(model2,
                   newdata = combinedData_sub1,
                   recompile = FALSE,
                   cores = 1,
                   chains = 1,
                   iter = iterPerChain,
                   save_all_pars = TRUE,
                   sample_prior = TRUE,
                   save_dso = TRUE,
                   seed = seed)
  return(list(i = i,
              seed = seed,
              model = model3))
}

# Create rslurm job 
sjob1 <- slurm_apply(model3_fun, pars, jobname = 'model3',
                     add_objects = c("model2", 'combinedData_sub1', 'iterPerChain'),
                     nodes = n_nodes, cpus_per_node = cpus_per_node, submit = FALSE)



# /* 
# ----------------------------- R + all others only correct (model 4) ---------------------------
# */
combinedData_sub2 <- combinedData[combinedData$accRecall == 1 | combinedData$remembered == 1, ] 

# rslurm function
model4_fun <- function(i, seed){
  model4 <- update(model2,
                   newdata = combinedData_sub2,
                   recompile = FALSE,
                   cores = 1,
                   chains = 1,
                   iter = iterPerChain,
                   save_all_pars = TRUE,
                   sample_prior = TRUE,
                   save_dso = TRUE,
                   seed = seed)
  return(list(i = i,
              seed = seed,
              model = model4))
}

# Create rslurm job 
sjob1 <- slurm_apply(model4_fun, pars, jobname = 'model4',
                     add_objects = c("model2", 'combinedData_sub2', 'iterPerChain'),
                     nodes = n_nodes, cpus_per_node = cpus_per_node, submit = FALSE)