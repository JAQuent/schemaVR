# This scripts generates a null distribution for BF for exhaustive searchlight test
# Date: 20/11/2020
# Explanation: ADD

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
# ----------------------------- Simulation functions --------------------------
# */
# Function used to generate data
data_generator <- function(numObj, numSub, beta0, beta1, beta2){
  # Give numObj obj true values from uniform distribution between -100 and 100.
  trueObj_val      <- runif(numObj, -100, 100)

  
  # For each subject draw a random value with the mean of the object with sd that scales with this value
  subjVal <- matrix(NA, numSub, numObj)
  
  for(i in 1:numSub){
    for(j in 1:numObj){
      subjVal[i, j] <- rnorm(1, trueObj_val[j], 100)
    }
  }
  
  # Capping at -100 and 100
  subjVal[subjVal > 100]  <- 100
  subjVal[subjVal < -100] <- -100
  
  # Create DF and also scale x 
  df <- data.frame(sub  = rep(1:numSub, numObj),
                   obj  = rep(1:numObj, each = numSub),
                   x    = c(subjVal))
  
  # Scaling based on https://github.com/stan-dev/stan/wiki/Prior-Choice-Recommendations
  # Mean = 0 and SD = 0.5
  df$s_x <- (df$x - mean(df$x))/(sd(df$x)/0.5)
  
  # Predicting memory
  z     <- beta0 + beta1*df$s_x + beta2*df$s_x*df$s_x # Regression in log odds
  pr    <- 1/(1+exp(-z)) # Convert to probability.
  df$y  <- rbinom(numObj*numSub, 1, pr)
  return(df)
}


# Priors for all models
priors  <- c(prior(student_t(7, 0, 10) , class = "Intercept"),
             prior(student_t(7, 0, 1) , class = "b")) 

priorDensity <- dstudent_t(0, 7, 0, 1)

# Generate data for base model
df              <- data_generator(20, 60, 0, 0, 0.5)


# Setting up DF for interrupted regression
breakingPoints <- 0 
x              <- df$s_x 
df$xlow        <- ifelse(x <= breakingPoints[1], x - breakingPoints[1], 0)
df$xhigh       <- ifelse(x > breakingPoints[1], x - breakingPoints[1], 0)     
df$high        <- ifelse(x > breakingPoints[1], 1, 0)

# Run 1 for model compilation
baseModel <- brm(y ~ xlow + xhigh + high,
                 data = df,
                 prior = priors,
                 family = bernoulli(),
                 chains = 1,
                 save_all_pars = TRUE,
                 sample_prior = TRUE,
                 save_dso = TRUE, 
                 seed = 6353) 


nullData_sim <- function(index, seed, numObj, numSub, beta0, beta1, beta2, br_range, numPoints){
  # Set seed 
  set.seed(seed)
  seeds <- sample(.Machine$integer.max, numPoints)
  
  #  Generate data
  df <- data_generator(numObj, numSub, beta0, beta1, beta2)
  
  # Possible breaking points that will be tested
  br_range_val   <- quantile(df$s_x, c((1 - br_range)/2, br_range + (1 - br_range)/2))
  breakingPoints <- seq(br_range_val[1], br_range_val[2], length.out =  numPoints)
  
  # Go through all breaking points
  x      <- df$s_x 
  for(i in 1:numPoints){
    # Slope 1 (breaking point is included in the first line)
    df$xlow     <- ifelse(x <= breakingPoints[i], x - breakingPoints[i], 0)
    df$xhigh    <- ifelse(x > breakingPoints[i], x - breakingPoints[i], 0)     
    df$high     <- ifelse(x > breakingPoints[i], 1, 0)
    
    # Model 1
    error1      <- FALSE
    model1      <- tryCatch({
      model1 <- update(baseModel,
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
                       seed = seeds[i])
    }, error = function(e){
      error1 <- TRUE
      return(as.character(e))
    })
    
    # Slope 2 (breaking point is included in the second line)
    df$xlow     <- ifelse(x < breakingPoints[i], x - breakingPoints[i], 0)
    df$xhigh    <- ifelse(x >= breakingPoints[i], x - breakingPoints[i], 0)     
    df$high     <- ifelse(x >= breakingPoints[i], 1, 0)
    
    # Model 2 
    error2      <- FALSE
    model2      <- tryCatch({
      model2 <- update(baseModel,
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
                       seed = seeds[i])
    }, error = function(e){
      error2 <- TRUE
      return(as.character(e))
    })
    
    # Extract information
    # Slope 1
    if(!error1){
      # Extract fixef
      slope1   <- fixef(model1)[2, 1]
      q2.5_1   <- fixef(model1)[2, 3]
      q97.5_1  <- fixef(model1)[2, 4]
      
      # Calculate BF manually
      fit.posterior  <- logspline(posterior_samples(model1)$b_xlow)
      posterior      <- dlogspline(0, fit.posterior) 
      prior          <- priorDensity # Precalculated density
      bf_1           <- prior/posterior
      
      # Calculate OR BF manually 
      areaPosterior <- sum(posterior_samples(model1)$b_xlow < 0)/length(posterior_samples(model1)$b_xlow)
      posterior.OR  <- posterior/areaPosterior    
      prior.OR      <- prior/0.5
      bf_OR_1       <- prior.OR/posterior.OR
      
      msg1 <- NA
    } else {
      slope1   <- NA
      q2.5_1   <- NA
      q97.5_1  <- NA
      bf_1     <- NA
      bf_OR_1  <- NA
      msg1     <- model1
    }

    # Slope 2
    # Extract fixef
    if(!error2){
      # Extract fixef
      slope2   <- fixef(model2)[3, 1]
      q2.5_2   <- fixef(model2)[3, 3]
      q97.5_2  <- fixef(model2)[3, 4]
      
      # Calculate BF manually
      fit.posterior  <- logspline(posterior_samples(model2)$b_xhigh)
      posterior      <- dlogspline(0, fit.posterior) 
      prior          <- priorDensity # Precalculated density
      bf_2           <- prior/posterior
      
      # Calculate OR BF manually 
      areaPosterior <- sum(posterior_samples(model2)$b_xhigh > 0)/length(posterior_samples(model2)$b_xhigh)
      posterior.OR  <- posterior/areaPosterior    
      prior.OR      <- prior/0.5
      bf_OR_2       <- prior.OR/posterior.OR
      
      msg2 <- NA
    } else {
      slope2   <- NA
      q2.5_2   <- NA
      q97.5_2  <- NA
      bf_2     <- NA
      bf_OR_2  <- NA
      msg2     <- model2
    }
    
    # Bind to one DF
    tempRessults <- data.frame(index          = index,
                               pointedTested  = i,
                               br_range       = br_range,
                               numPoints      = numPoints,
                               numObj         = numObj,
                               numSub         = numSub,
                               beta0          = beta0, 
                               beta1          = beta1,
                               beta2          = beta2,
                               breakingPoint  = breakingPoints[i],
                               slope1         = slope1,
                               q2.5_1         = q2.5_1,
                               q97.5_1        = q97.5_1,
                               bf_1           = bf_1,
                               bf_OR_1        = bf_OR_1,
                               slope2         = slope2,
                               q2.5_2         = q2.5_2,
                               q97.5_2        = q97.5_2,
                               bf_2           = bf_2,
                               bf_OR_2        = bf_OR_2,
                               msg1           = msg1,
                               msg2           = msg2)
    
    
    # Add to df 
    if(i == 1){
      # Get results
      results <- tempRessults
    } else {
      results <- rbind(results, tempRessults)
    }
  }
  
  rownames(results) <- NULL
  return(results)
}

# Job parameters
n_nodes       <- 30
cpus_per_node <- 22
iterPerChain  <- 9000

# Parameters of simulation
nIter      <- 100000
numBeta    <- 1
totalIter  <- nIter*numBeta
seeds      <- sample(.Machine$integer.max, totalIter)
numObj     <- rep(20, numBeta) # Number of objects
numSub     <- rep(94, numBeta) # Number of subjects
beta0      <- rep(0, numBeta)
beta1      <- rep(0, numBeta)
beta2      <- rep(0, numBeta)# seq(0, 0.7, length.out =  numBeta) #  rep(0, numBeta)
br_range   <- rep(0.8, numBeta) # middle % range of accept s_x values for breaking point
numPoints  <- rep(10, numBeta)

params <- data.frame(index = 1:totalIter,
                     seed = seeds,
                     numObj = rep(numObj, nIter),
                     numSub = rep(numSub, nIter),
                     beta0 = rep(beta0, nIter),
                     beta1 = rep(beta1, nIter),
                     beta2 = rep(beta2, nIter),
                     br_range = rep(br_range, nIter),
                     numPoints = rep(numPoints, nIter))

# Submitting job
jobName <- "nullData_BF"
sjob1 <- slurm_apply(nullData_sim, params, jobname = jobName,
                     add_objects = c('data_generator', 'baseModel', 'iterPerChain', 'priors', 'priorDensity'),
                     nodes = n_nodes, cpus_per_node = cpus_per_node, slurm_options = list(time = "2-12"), submit = TRUE)


# Saving history for reproducibility to _rslurm_ folder
duplicateScript(paste0("_rslurm_", jobName, "/"))