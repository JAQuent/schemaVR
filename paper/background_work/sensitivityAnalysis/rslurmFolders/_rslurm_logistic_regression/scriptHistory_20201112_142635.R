# Simulation for priors in logistic regression
# Date: 12/11/2020
# Explanation: Gelman suggests beta ~ student_t(nu,0,s) where s is chosen to provide weak information on the expected scale, and 3<nu<7.

# Setting seed
set.seed(244)

# /*
# ----------------------------- Libraries --------------------------
# */
# Set library path
.libPaths('/home/aq01/R/x86_64-redhat-linux-gnu-library/3.5')

library(brms)
library(rslurm)
library(polspline)
library(assortedRFunctions)


# /*
# ----------------------------- Data function --------------------------
# */
# Function used to generate data
data_generator <- function(numSub, beta0, beta1){
  # Create DF and also scale x 
  df <- data.frame(sub  = rep(1:numSub),
                   x    = rnorm(numSub))
  
  # Scaling based on https://github.com/stan-dev/stan/wiki/Prior-Choice-Recommendations
  # Mean = 0 and SD = 0.5
  df$s_x <- (df$x - mean(df$x))/(sd(df$x)/0.5)
  
  # Predicting memory
  z     <- beta0 + beta1*df$s_x # Regression in log odds
  pr    <- 1/(1+exp(-z)) # Convert to probability.
  df$y  <- rbinom(numSub, 1, pr)
  return(df)
}


# /*
# ----------------------------- Priors --------------------------
# */
priors_3  <- c(prior(student_t(3, 0, 10) , class = "Intercept"),
               prior(student_t(3, 0, 2.5) , class = "b")) 

priors_4  <- c(prior(student_t(4, 0, 10) , class = "Intercept"),
               prior(student_t(4, 0, 2.5) , class = "b")) 

priors_5  <- c(prior(student_t(5, 0, 10) , class = "Intercept"),
               prior(student_t(5, 0, 2.5) , class = "b")) 

priors_6  <- c(prior(student_t(6, 0, 10) , class = "Intercept"),
               prior(student_t(6, 0, 2.5) , class = "b")) 

priors_7  <- c(prior(student_t(7, 0, 10) , class = "Intercept"),
               prior(student_t(7, 0, 2.5) , class = "b")) 

priorDensity_3 <- dstudent_t(0, 3, 0, 2.5)
priorDensity_4 <- dstudent_t(0, 4, 0, 2.5)
priorDensity_5 <- dstudent_t(0, 5, 0, 2.5)
priorDensity_6 <- dstudent_t(0, 6, 0, 2.5)
priorDensity_7 <- dstudent_t(0, 7, 0, 2.5)


# /*
# ----------------------------- Base models --------------------------
# */
# Create baseModels
df <- data_generator(80, 0, 1)

# Run the models for compilation
baseModel_3 <- brm(y ~ s_x,
                   data = df,
                   prior = priors_3,
                   family = bernoulli(),
                   chains = 1,
                   save_all_pars = TRUE,
                   sample_prior = TRUE,
                   save_dso = TRUE, 
                   seed = 6353) 

baseModel_4 <- brm(y ~ s_x,
                   data = df,
                   prior = priors_4,
                   family = bernoulli(),
                   chains = 1,
                   save_all_pars = TRUE,
                   sample_prior = TRUE,
                   save_dso = TRUE, 
                   seed = 6353) 

baseModel_5 <- brm(y ~ s_x,
                   data = df,
                   prior = priors_5,
                   family = bernoulli(),
                   chains = 1,
                   save_all_pars = TRUE,
                   sample_prior = TRUE,
                   save_dso = TRUE, 
                   seed = 6353) 

baseModel_6 <- brm(y ~ s_x,
                   data = df,
                   prior = priors_6,
                   family = bernoulli(),
                   chains = 1,
                   save_all_pars = TRUE,
                   sample_prior = TRUE,
                   save_dso = TRUE, 
                   seed = 6353) 

baseModel_7 <- brm(y ~ s_x,
                   data = df,
                   prior = priors_7,
                   family = bernoulli(),
                   chains = 1,
                   save_all_pars = TRUE,
                   sample_prior = TRUE,
                   save_dso = TRUE, 
                   seed = 6353) 



# /*
# ----------------------------- Simulation function --------------------------
# */
sim_function <- function(index, seed, numSub, beta0, beta1, prior){
  #  Generate data
  df <- data_generator(numSub, beta0, beta1)
  
  # Get model
  baseModel <- get(paste0('baseModel_', prior))
  
  # Run model in tryCatch function
  error      <- FALSE
  model      <- tryCatch({
    model <- update(baseModel,
                     newdata = df,
                     recompile = FALSE,
                     cores = 1,
                     chains = 1,
                     iter = iterPerChain,
                     warmup = 3000,
                     save_all_pars = TRUE,
                     sample_prior = TRUE,
                     save_dso = TRUE,
                     silent = TRUE,
                     refresh = 0,
                     seed = seed)
  }, error = function(e){
    error <- TRUE
    return(as.character(e))
  })

  # Calculate BF if no error  
    if(!error){
      # Extract fixef
      slope  <- fixef(model)[2, 1]
      q2.5   <- fixef(model)[2, 3]
      q97.5  <- fixef(model)[2, 4]
      
      # Get prior density
      priorDensity <- get(paste0('priorDensity_', prior))
      
      # Calculate BF manually
      fit.posterior  <- logspline(posterior_samples(model)$b_s_x)
      posterior      <- dlogspline(0, fit.posterior) 
      prior          <- priorDensity # Precalculated density
      bf             <- prior/posterior
      
      # Calculate OR BF manually 
      areaPosterior <- sum(posterior_samples(model)$b_s_x > 0)/length(posterior_samples(model)$b_s_x)
      posterior.OR  <- posterior/areaPosterior    
      prior.OR      <- prior/0.5
      bf_OR         <- prior.OR/posterior.OR
      
      msg    <- NA
    } else {
      slope  <- NA
      q2.5   <- NA
      q97.5  <- NA
      bf     <- NA
      bf_OR  <- NA
      msg    <- model
    }
    
    # Bind to one DF
    results <- data.frame(index  = index,
                          numSub = numSub,
                          beta0  = beta0, 
                          beta1  = beta1,
                          slope  = slope,
                          q2.5   = q2.5,
                          q97.5  = q97.5,
                          bf     = bf,
                          bf_OR  = bf_OR,
                          msg    = msg)
    
  
  rownames(results) <- NULL
  return(results)
}


# /*
# ----------------------------- Job parameters --------------------------
# */
# Job parameters
n_nodes       <- 30
cpus_per_node <- 22
iterPerChain  <- 9000

# Parameters of simulation
nIter      <- 1000
numBeta    <- 6
numPrior   <- 5
totalIter  <- nIter*numBeta*numPrior
seeds      <- sample(.Machine$integer.max, totalIter)
numSub     <- rep(94, numBeta*numPrior) # Number of subjects
beta0      <- rep(0, numBeta*numPrior)
beta1      <- seq(0, 0.7, length.out =  numBeta)
prior      <- rep(3:7, each = numBeta)


params <- data.frame(index = 1:totalIter,
                     seed = seeds,
                     numSub = rep(numSub, nIter),
                     beta0 = rep(beta0, nIter),
                     beta1 = rep(beta1, nIter),
                     prior = rep(prior, nIter))

# /*
# ----------------------------- Submitting job --------------------------
# */
jobName <- "logistic_regression"
sjob1 <- slurm_apply(sim_function, params, jobname = jobName,
                     add_objects = c('data_generator', 'baseModel_3', 'baseModel_4', 'baseModel_5', 'baseModel_6', 'baseModel_7', 
                                     'iterPerChain', 'priors_3', 'priors_4', 'priors_5', 'priors_6', 'priors_7', 'priorDensity_3',
                                     'priorDensity_3','priorDensity_4', 'priorDensity_5', 'priorDensity_6', 'priorDensity_7'),
                     nodes = n_nodes, cpus_per_node = cpus_per_node, slurm_options = list(time = "2-12"), submit = TRUE)

# Saving history for reproducibility to _rslurm_ folder
duplicateScript(paste0("_rslurm_", jobName, "/"))