# Script to simulate two line test with only one breaking point
# Version 1.0
# Date: 29/10/2019
# Author: Joern Alexander Quent
# /*
# ----------------------------- Libraries, settings and functions ---------------------------
# */
# Setting seed and cores
seed <- sample(100200)
set.seed(seed)
cores2use <- 2

# Libraries
library(brms)
library(assortedRFunctions)

# Simulation parameters
waitTime   <- 1.1
nSims      <- 50
n          <- 1142 # based on number of recall data 
br         <- seq(-1.681162, 1.681162,  length.out =  17)
nBr        <- length(br)
nRunsTotal <- nSims*nBr
seeds      <- sample(9999999, nRunsTotal)
priors     <- c(prior(normal(0, 1) , class = "Intercept"),
                prior(normal(0, 1) , class = "b", coef = 'high'),
                prior(normal(0, 1) , class = "b", coef = 'xlow'),
                prior(normal(0, 1)  , class = "b", coef = 'xhigh'))
index   <- 1
results <- list()

# /*
# ----------------------------- Simulation --------------------------
# */
# Loop
startTime <- Sys.time()
for(j in 1:nSims){
  # One data draw per simulation run
  df       <- data.frame(x = rnorm(n), y = rnorm(n))
  
  for(i in 1:nBr){
    if(index == 1){
      # First run
      df$xlow  <- ifelse(df$x <= br[i], df$x - br[i], 0)
      df$xhigh <- ifelse(df$x > br[i], df$x - br[i], 0)     
      df$high  <- ifelse(df$x > br[i], 1, 0)
      
      # Compile and run model
      model    <- brm(y ~ xlow + xhigh + high,
                      data = df,
                      prior = priors,
                      cores = cores2use,
                      save_all_pars = TRUE,
                      sample_prior = TRUE,
                      seed = seeds[index])
      results[[index]] <- list(br = br[i], effect = fixef(model))
      
      # sleep for 1 sec to avoid crashing of r sessions
      Sys.sleep(waitTime) 
      
      # Progress update and predicted time printed to console
      currentTime <- Sys.time()
      elapsedTime   <- currentTime - startTime
      predictedTime <- elapsedTime * (1/(index/nRunsTotal))
      cat(paste('Runs completed:', 
                index, 
                'Expected finish: ',
                as.character(startTime + predictedTime),
                '\n'))
      
      # Update index
      index   <- index + 1
      
    } else {
      df$xlow  <- ifelse(df$x <= br[i], df$x - br[i], 0)
      df$xhigh <- ifelse(df$x > br[i], df$x - br[i], 0)     
      df$high  <- ifelse(df$x > br[i], 1, 0)
      
      # Run model
      model    <- update(model,
                         newdata = df,
                         recompile = FALSE,
                         cores = cores2use,
                         save_all_pars = TRUE,
                         sample_prior = TRUE,
                         seed = seeds[index])
      # sleep for 1 sec to avoid crashing of r sessions
      Sys.sleep(waitTime) 
      
      # Progress update and predicted time printed to console
      currentTime <- Sys.time()
      elapsedTime   <- currentTime - startTime
      predictedTime <- elapsedTime * (1/(index/nRunsTotal))
      cat(paste('Runs completed:', 
                index, 
                'Expected finish: ',
                as.character(startTime + predictedTime),
                '\n'))
      results[[index]] <- list(br = br[i], effect = fixef(model))
      
      # Update index
      index   <- index + 1
    }
  }
}

# /*
# ----------------------------- Saving image ---------------------------
# */
save.image(datedFileNam('twoLineSimulation_searchLight', '.RData'))