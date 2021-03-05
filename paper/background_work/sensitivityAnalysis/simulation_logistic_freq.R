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

library(plyr)
library(rslurm)
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


# ----------------------------- Simulation function --------------------------
# */
sim_function <- function(index, seed, numSub, beta0, beta1){
  #  Generate data
  df <- data_generator(numSub, beta0, beta1)
  
  # Run model in tryCatch function
  error      <- FALSE
  model      <- tryCatch({
    model <- glm(y ~ s_x, data = df, family = "binomial")
    
  }, error = function(e){
    error <- TRUE
    return(as.character(e))
  })

  # Extract all information if no error  
    if(!error){
      # Get summary model
      model_summary <- summary(model)$coefficients
      
      # Intercept
      int_est      <- model_summary[1, 1]
      int_est.err  <- model_summary[1, 2]
      int_z        <- model_summary[1, 3]
      int_p        <- model_summary[1, 4]
      
      # Slope
      slope_est      <- model_summary[2, 1]
      slope_est.err  <- model_summary[2, 2]
      slope_z        <- model_summary[2, 3]
      slope_p        <- model_summary[2, 4]
      
      # Set msg to NA because no problem
      msg    <- NA
    } else {
      # Intercept
      int_est      <- NA
      int_est.err  <- NA
      int_z        <- NA
      int_p        <- NA
      
      # Slope
      slope_est      <- NA
      slope_est.err  <- NA
      slope_z        <- NA
      slope_p        <- NA
      
      # Save error message that is saved ot model if something goes wrong
      msg    <- model
    }
    
    # Bind to one DF
    results <- data.frame(index         = index,
                          numSub        = numSub,
                          beta0         = beta0, 
                          beta1         = beta1,
                          int_est       = int_est,
                          int_est.err   = int_est.err,
                          int_z         = int_z,
                          int_p         = int_p,
                          slope_est     = slope_est,
                          slope_est.err = slope_est.err,
                          slope_z       = slope_z,
                          slope_p       = slope_p,
                          msg           = msg)
    
  
  rownames(results) <- NULL
  return(results)
}


# /*
# ----------------------------- Job parameters --------------------------
# */
# Job parameters
n_nodes       <- 20
cpus_per_node <- 22

# Parameters of simulation
nIter         <- 10000
numBeta       <- 8
totalIter     <- nIter*numBeta
seeds         <- sample(.Machine$integer.max, totalIter)
numSub        <- rep(100, totalIter) # Number of subjects
beta0         <- rep(0, totalIter)
beta1         <- seq(0, 2, length.out =  numBeta)

params <- data.frame(index = 1:totalIter,
                     seed = seeds,
                     numSub = numSub,
                     beta0 = beta0,
                     beta1 = rep(beta1, nIter))

# /*
# ----------------------------- Submitting job --------------------------
# */
jobName <- "logistic_regression_freq"
sjob1 <- slurm_apply(sim_function, params, jobname = jobName,
                     add_objects = c('data_generator'),
                     nodes = n_nodes, cpus_per_node = cpus_per_node, slurm_options = list(time = "2-12"), submit = TRUE)

# Saving history for reproducibility to _rslurm_ folder
duplicateScript(paste0("_rslurm_", jobName, "/"))