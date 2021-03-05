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
library(posterior)
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
#
# Student prior
# 1
priors_student_1  <- c(prior(student_t(7, 0, 10) , class = "Intercept"),
               prior(student_t(7, 0, 1) , class = "b")) 
priorDensity_student_1 <- dstudent_t(0, 7, 0, 1)

# 1.5
priors_student_1.5  <- c(prior(student_t(7, 0, 10) , class = "Intercept"),
                 prior(student_t(7, 0, 1.5) , class = "b")) 
priorDensity_student_1.5 <- dstudent_t(0, 7, 0, 1.5)

# 2
priors_student_2  <- c(prior(student_t(7, 0, 10) , class = "Intercept"),
               prior(student_t(7, 0, 2) , class = "b")) 
priorDensity_student_2 <- dstudent_t(0, 7, 0, 2)

# 2.5
priors_student_2.5  <- c(prior(student_t(7, 0, 10) , class = "Intercept"),
                 prior(student_t(7, 0, 2.5) , class = "b")) 
priorDensity_student_2.5 <- dstudent_t(0, 7, 0,2.5)

# Normal prior
# 1
priors_normal_1  <- c(prior(normal(0, 10) , class = "Intercept"),
                      prior(normal(0, 1) , class = "b")) 
priorDensity_normal_1 <- dnorm(0, 0, 1)

# 1.5
priors_normal_1.5  <- c(prior(normal(0, 10) , class = "Intercept"),
                        prior(normal(0, 1.5) , class = "b")) 
priorDensity_normal_1.5 <- dnorm(0, 0, 1.5)

# 2
priors_normal_2  <- c(prior(normal(0, 10) , class = "Intercept"),
                      prior(normal(0, 2) , class = "b")) 
priorDensity_normal_2 <- dnorm(0, 0, 2)

# 2.5
priors_normal_2.5  <- c(prior(normal(0, 10) , class = "Intercept"),
                        prior(normal(0, 2.5) , class = "b")) 
priorDensity_normal_2.5 <- dnorm(0, 0, 2.5)

# /*
# ----------------------------- Base models --------------------------
# */
# Create baseModels
df <- data_generator(80, 0, 1)

# Run the models for compilation
baseModel_student_1 <- brm(y ~ s_x,
                   data = df,
                   prior = priors_student_1,
                   family = bernoulli(),
                   chains = 1,
                   save_all_pars = TRUE,
                   sample_prior = TRUE,
                   save_dso = TRUE, 
                   seed = 6353) 

baseModel_student_1.5 <- brm(y ~ s_x,
                           data = df,
                           prior = priors_student_1.5,
                           family = bernoulli(),
                           chains = 1,
                           save_all_pars = TRUE,
                           sample_prior = TRUE,
                           save_dso = TRUE, 
                           seed = 6353) 

baseModel_student_2 <- brm(y ~ s_x,
                           data = df,
                           prior = priors_student_2,
                           family = bernoulli(),
                           chains = 1,
                           save_all_pars = TRUE,
                           sample_prior = TRUE,
                           save_dso = TRUE, 
                           seed = 6353)

baseModel_student_2.5 <- brm(y ~ s_x,
                           data = df,
                           prior = priors_student_2.5,
                           family = bernoulli(),
                           chains = 1,
                           save_all_pars = TRUE,
                           sample_prior = TRUE,
                           save_dso = TRUE, 
                           seed = 6353) 

baseModel_normal_1 <- brm(y ~ s_x,
                          data = df,
                          prior = priors_normal_1,
                          family = bernoulli(),
                          chains = 1,
                          save_all_pars = TRUE,
                          sample_prior = TRUE,
                          save_dso = TRUE, 
                          seed = 6353) 

baseModel_normal_1.5 <- brm(y ~ s_x,
                            data = df,
                            prior = priors_normal_1.5,
                            family = bernoulli(),
                            chains = 1,
                            save_all_pars = TRUE,
                            sample_prior = TRUE,
                            save_dso = TRUE, 
                            seed = 6353) 

baseModel_normal_2 <- brm(y ~ s_x,
                          data = df,
                          prior = priors_normal_2,
                          family = bernoulli(),
                          chains = 1,
                          save_all_pars = TRUE,
                          sample_prior = TRUE,
                          save_dso = TRUE, 
                          seed = 6353)

baseModel_normal_2.5 <- brm(y ~ s_x,
                            data = df,
                            prior = priors_normal_2.5,
                            family = bernoulli(),
                            chains = 1,
                            save_all_pars = TRUE,
                            sample_prior = TRUE,
                            save_dso = TRUE,
                            seed = 6353)

# /*
# ----------------------------- Simulation function --------------------------
# */
sim_function <- function(index, seed, numSub, beta0, beta1, type, scalePar){
  #  Generate data
  df <- data_generator(numSub, beta0, beta1)
  
  # Get model
  baseModel <- get(paste0('baseModel_', type, '_', scalePar))
  
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
      # Extract information from model
      model_summary <- summary(model)$fixed
      
      # Extract posterior distribution to calculate ESS and BF
      postDist_int   <- posterior_samples(model)$b_Intercept
      postDist_slope <- posterior_samples(model)$b_s_x
      
      # Intercept
      int_est      <- model_summary[1, 1]
      int_est.err  <- model_summary[1, 2]
      int_l_CI     <- model_summary[1, 3]
      int_u_CI     <- model_summary[1, 4]
      int_rhat     <- model_summary[1, 5]
      int_bulk_ess <- ess_bulk(postDist_int)
      int_tail_ess <- ess_tail(postDist_int)
      
      # Slope
      slope_est      <- model_summary[2, 1]
      slope_est.err  <- model_summary[2, 2]
      slope_l_CI     <- model_summary[2, 3]
      slope_u_CI     <- model_summary[2, 4]
      slope_rhat     <- model_summary[2, 5]
      slope_bulk_ess <- ess_bulk(postDist_slope)
      slope_tail_ess <- ess_tail(postDist_slope)
        
      # Get prior density
      priorDensity <- get(paste0('priorDensity_', type, '_', scalePar))
      
      # Calculate BF manually
      fit.posterior  <- logspline(postDist_slope)
      posterior      <- dlogspline(0, fit.posterior) 
      prior          <- priorDensity # Precalculated density
      bf             <- prior/posterior
      
      # Calculate OR BF manually 
      areaPosterior <- sum(postDist_slope > 0)/length(postDist_slope)
      posterior.OR  <- posterior/areaPosterior    
      prior.OR      <- prior/0.5
      bf_OR         <- prior.OR/posterior.OR
      
      # Extract ESS
      
      msg    <- NA
    } else {
      slope  <- NA
      q2.5   <- NA
      q97.5  <- NA
      bf     <- NA
      bf_OR  <- NA
      ESS    <- NA
      msg    <- model
    }
    
    # Bind to one DF
    results <- data.frame(index          = index,
                          numSub         = numSub,
                          beta0          = beta0, 
                          beta1          = beta1,
                          type           = type,
                          scalePar       = scalePar,
                          int_est        = int_est,
                          int_est.err    = int_est.err,
                          int_l_CI       = int_l_CI,
                          int_u_CI       = int_u_CI,
                          int_rhat       = int_rhat,
                          int_bulk_ess   = int_bulk_ess,
                          int_tail_ess   = int_tail_ess,
                          bf10           = bf,
                          bf10_OR        = bf_OR,
                          slope_est      = slope_est,
                          slope_est.err  = slope_est.err,
                          slope_l_CI     = slope_l_CI,
                          slope_u_CI     = slope_u_CI,
                          slope_rhat     = slope_rhat,
                          slope_bulk_ess = slope_bulk_ess,
                          slope_tail_ess = slope_tail_ess,
                          msg            = msg)
    
  
  rownames(results) <- NULL
  return(results)
}


# /*
# ----------------------------- Job parameters --------------------------
# */
# Job parameters
n_nodes       <- 40
cpus_per_node <- 22
iterPerChain  <- 100000

# Parameters of simulation
nIter         <- 100
scalePars     <- c('1', '1.5', '2', '2.5')
types         <- c('student', 'normal')
numBeta       <- 8
num_scalePar  <- length(scalePars)
numType       <- length(types)
totalIter     <- nIter*numBeta*num_scalePar*numType
seeds         <- sample(.Machine$integer.max, totalIter)
numSub        <- rep(100, totalIter) # Number of subjects
beta0         <- rep(0, totalIter)
beta1         <- seq(0, 2, length.out =  numBeta)
expanded      <- expand.grid(beta1, types, scalePars)

params <- data.frame(index = 1:totalIter,
                     seed = seeds,
                     numSub = numSub,
                     beta0 = beta0,
                     beta1 = rep(expanded$Var1, nIter),
                     type  = rep(expanded$Var2, nIter),
                     scalePar = rep(expanded$Var3, nIter))

# ddply(params, c('beta1','type', 'scalePar'), summarise, n = length(seed))

# /*
# ----------------------------- Submitting job --------------------------
# */
jobName <- "logistic_regression3"
sjob1 <- slurm_apply(sim_function, params, jobname = jobName,
                     add_objects = c('data_generator', 'baseModel_student_1', 'baseModel_student_1.5', 'baseModel_student_2', 'baseModel_student_2.5', 
                                     'baseModel_normal_1', 'baseModel_normal_1.5', 'baseModel_normal_2', 'baseModel_normal_2.5',
                                     'iterPerChain', 'priorDensity_student_1', 'priorDensity_student_1.5', 'priorDensity_student_2', 'priorDensity_student_2.5', 
                                     'priorDensity_normal_1', 'priorDensity_normal_1.5', 'priorDensity_normal_2','priorDensity_normal_2.5'),
                     nodes = n_nodes, cpus_per_node = cpus_per_node, slurm_options = list(time = "2-12"), submit = TRUE)

# Saving history for reproducibility to _rslurm_ folder
duplicateScript(paste0("_rslurm_", jobName, "/"))