# Script to simulate two line test with only one breaking point
# Version 1.0
# Date: 29/10/2019
# Author: Joern Alexander Quent
# /*
# ----------------------------- Libraries, settings and functions ---------------------------
# */
# Setting seed and cores
set.seed(4354)

# Libraries
library(brms)
library(rslurm)

# Simulation parameters
nSims      <- 20000
n          <- 1142 # based on number of recall data 
br         <- seq(-1.681162, 1.681162,  length.out =  17)
nBr        <- length(br)
priors     <- c(prior(normal(0, 1), class = "Intercept"),
                prior(normal(0, 1), class = "b", coef = 'high'),
                prior(normal(0, 1), class = "b", coef = 'xlow'),
                prior(normal(0, 1), class = "b", coef = 'xhigh'))


pars <- data.frame(iter = 1:nSims,
                   n = rep(n, nSims),
                   seed = sample(9999999, nSims))

# /*
# ----------------------------- Run model for compilation --------------------------
# */
df1       <- data.frame(x = rnorm(n), y = rnorm(n))
df1$xlow  <- ifelse(df1$x <= br[1], df1$x - br[1], 0)
df1$xhigh <- ifelse(df1$x > br[1], df1$x - br[1], 0)     
df1$high  <- ifelse(df1$x > br[1], 1, 0)

# Compile and run model
model    <- brm(y ~ xlow + xhigh + high,
                data = df1,
                prior = priors,
                silent = TRUE,
                seed = pars$seed[1])


# /*
# ----------------------------- Function  for rslurm --------------------------
# */
twolineSim <- function(iter, seed, n){
  # Set seed for this iteration
  set.seed(seed)
  seeds <- sample(9999, nBr)
  df    <- data.frame(x = rnorm(n), y = rnorm(n))
  results <- list()
  
  for(i in 1:nBr){
    if(i == 1 & iter == 1){
      # Use compilation model
      res <- fixef(model)
      results[[i]] <- list(iter = iter, br = br[i], fixef = res)
    } else {
      if(iter == 1){
        # use df1 generated for compilation model
        df1$xlow  <- ifelse(df1$x <= br[i], df1$x - br[i], 0)
        df1$xhigh <- ifelse(df1$x > br[i], df1$x - br[i], 0)     
        df1$high  <- ifelse(df1$x > br[i], 1, 0)
        res <- fixef(update(model,
                     newdata = df1,
                     recompile = FALSE,
                     silent = TRUE,
                     seed = seeds[i]))
        results[[i]] <- list(iter = iter, br = br[i], fixef = res)
      } else {
        # Generate completely new data 
        df$xlow  <- ifelse(df$x <= br[i], df$x - br[i], 0)
        df$xhigh <- ifelse(df$x > br[i], df$x - br[i], 0)     
        df$high  <- ifelse(df$x > br[i], 1, 0)
        res <- fixef(update(model,
                     newdata = df,
                     recompile = FALSE,
                     silent = TRUE,
                     seed = seeds[i]))
        results[[i]] <- list(iter = iter, br = br[i], fixef = res)
      }
    }
  }
  return(results)
}

# /*
# ----------------------------- Create rslurm job --------------------------
# */
# Job parameters
n_nodes       <- 4
cpus_per_node <- 24

sjob <- slurm_apply(twolineSim, pars, jobname = 'twoline_searchLight_sim2',
                    add_objects = c("model", 'df1', 'br', 'nBr'),
                    nodes = n_nodes, cpus_per_node = cpus_per_node, submit = FALSE)