# Script to run analysis of AFC accuracy data for schemaVR1
# Version 1.0
# Date:  18/11/2019
# Author: Joern Alexander Quent
# /* 
# ----------------------------- Libraries, settings and functions ---------------------------
# */
# Setting seed
seed <- 13846

# Libaries
library(brms)
library(rslurm)

# /*
# ----------------------------- Parameters for analysis ---------------------------
# */
# BRMS parameters
chains       <- 1
nRuns        <- 8
iterPerChain <- 4000
seeds        <- sample(99999, nRuns)

# Job parameters
n_nodes       <- 1
cpus_per_node <- nRuns # 1 cpu per run

# /* 
# ----------------------------- Preparing data and priors ---------------------------
# */
# Loading data
load("/home/aq01/Projects/schemaVR/schemaVR1/data/dataSchemaVR1_cleaned.RData")

# Scaling based on Gelman et al. (2008) and https://github.com/stan-dev/stan/wiki/Prior-Choice-Recommendations
# Mean = 0 and SD = 0.5
dataSchemaVR1_AFC$Exp  <- dataSchemaVR1_AFC$objLocTargetRating 
dataSchemaVR1_AFC$sExp <- (dataSchemaVR1_AFC$Exp - mean(dataSchemaVR1_AFC$Exp ))/(sd(dataSchemaVR1_AFC$Exp)/0.5)

# Based on Gelman et al. (2008) and https://github.com/stan-dev/stan/wiki/Prior-Choice-Recommendations
prior_schemaVR1  <- c(prior(student_t(3, 0, 10) , class = "Intercept"),
                      prior(student_t(3, 0, 2.5) , class = "b")) 

# /* 
# ----------------------------- Model with  random slope and intercept ---------------------------
# */
# Parameter data frame 
pars <- data.frame(i = 1:nRuns, seed = seeds)

# Run 1 for model compilation
model_schemaVR1_AFC1 <- brm(accAFC ~ sExp +  
                             I(sExp*sExp) +
                             (sExp + I(sExp*sExp) | subNum) +
                             (sExp + I(sExp*sExp) | objNum),
                           data = dataSchemaVR1_AFC,
                           prior = prior_schemaVR1,
                           family = bernoulli(),
                           cores = 1,
                           chains = 1,
                           iter = iterPerChain,
                           save_all_pars = TRUE,
                           sample_prior = TRUE,
                           seed = pars[1, 'seed'])

# rslurm function
model1_fun <- function(i, seed){
  if(i == 1){
    return(list(i = i,
                seed = seed,
                model = model_schemaVR1_AFC1))
  } else {
    model_schemaVR1_AFC1 <- update(model_schemaVR1_AFC1,
                           newdata = dataSchemaVR1_AFC,
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
                model = model_schemaVR1_AFC1))
  }
  
}

# Create rslurm job 
sjob1 <- slurm_apply(model1_fun, pars, jobname = 'schemaVR1_AFC_uniformed_randomSlope',
                     add_objects = c("model_schemaVR1_AFC1", 'dataSchemaVR1_AFC', 'prior_schemaVR1', 'iterPerChain'),
                     nodes = n_nodes, cpus_per_node = cpus_per_node, submit = FALSE)


# /* 
# ----------------------------- Model with  random slope and intercept ---------------------------
# */
seeds <- sample(99999, nRuns)
pars  <- data.frame(i = 1:nRuns, seed = seeds)

# Run 1 for model compilation
model_schemaVR1_AFC2 <- brm(accAFC ~ sExp +  
                                I(sExp*sExp) +
                                (1 | subNum) +
                                (1 | objNum),
                            data = dataSchemaVR1_AFC,
                            prior = prior_schemaVR1,
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
                model = model_schemaVR1_AFC2))
  } else {
    model_schemaVR1_AFC2 <- update(model_schemaVR1_AFC2,
                                   newdata = dataSchemaVR1_AFC,
                                   recompile = FALSE,
                                   cores = 1,
                                   chains = 1,
                                   iter = iterPerChain,
                                   save_all_pars = TRUE,
                                   save_dso = TRUE, 
                                   sample_prior = TRUE,
                                   seed = seed)
    return(list(i = i,
                seed = seed,
                model = model_schemaVR1_AFC2))
  }
  
}



# Create rslurm job 
sjob1 <- slurm_apply(model2_fun, pars, jobname = 'schemaVR1_AFC_uniformed_randomIntercept',
                     add_objects = c("model_schemaVR1_AFC2", 'dataSchemaVR1_AFC', 'prior_schemaVR1', 'iterPerChain'),
                     nodes = n_nodes, cpus_per_node = cpus_per_node, submit = FALSE)