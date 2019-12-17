# Script to run analysis of AFC accuracy data for schemaVR2
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
load("/home/aq01/Projects/schemaVR/schemaVR2/data/dataSchemaVR2_cleaned.RData")

# Scaling based on Gelman et al. (2008) and https://github.com/stan-dev/stan/wiki/Prior-Choice-Recommendations
# Mean = 0 and SD = 0.5
dataSchemaVR2_AFC$Exp  <- dataSchemaVR2_AFC$objLocTargetRating 
dataSchemaVR2_AFC$sExp <- (dataSchemaVR2_AFC$Exp - mean(dataSchemaVR2_AFC$Exp ))/(sd(dataSchemaVR2_AFC$Exp)/0.5)

# Based on Gelman et al. (2008) and https://github.com/stan-dev/stan/wiki/Prior-Choice-Recommendations
prior_schemaVR2  <- c(prior(student_t(3, 0, 10) , class = "Intercept"),
                      prior(student_t(3, 0, 2.5) , class = "b")) 

# /* 
# ----------------------------- Model with random intercept ---------------------------
# */
pars  <- data.frame(i = 1:nRuns, seed = seeds)

# Run 1 for model compilation
model_schemaVR2_AFC <- brm(accAFC ~ sExp +  
                                I(sExp*sExp) +
                                (1 | subNum) +
                                (1 | objNum),
                            data = dataSchemaVR2_AFC,
                            prior = prior_schemaVR2,
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
                model = model_schemaVR2_AFC))
  } else {
    model_schemaVR2_AFC <- update(model_schemaVR2_AFC,
                                   newdata = dataSchemaVR2_AFC,
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
                model = model_schemaVR2_AFC))
  }
  
}



# Create rslurm job 
sjob1 <- slurm_apply(model2_fun, pars, jobname = 'schemaVR2_AFC_uniformed',
                     add_objects = c("model_schemaVR2_AFC", 'dataSchemaVR2_AFC', 'prior_schemaVR2', 'iterPerChain'),
                     nodes = n_nodes, cpus_per_node = cpus_per_node, submit = FALSE)