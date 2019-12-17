# Script to simulate two line test with only one breaking point
# Version 1.0
# Date: 29/10/2019
# Author: Joern Alexander Quent
# /*
# ----------------------------- Libraries, settings and functions ---------------------------
# */
# Setting seed and cores
set.seed(1544)

# Libraries
library(brms)
library(rslurm)

# Simulation parameters
nSims      <- 10000
n          <- 1142 # based on number of recall data 
br         <- 0
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
df       <- data.frame(x = rnorm(n), y = rnorm(n))
df$xlow  <- ifelse(df$x <= br, df$x - br, 0)
df$xhigh <- ifelse(df$x > br, df$x - br, 0)     
df$high  <- ifelse(df$x > br, 1, 0)

# Compile and run model
model    <- brm(y ~ xlow + xhigh + high,
                data = df,
                prior = priors,
                silent = TRUE,
                seed = pars$seed[1])


# /*
# ----------------------------- Function  for rslurm --------------------------
# */
twolineSim <- function(iter, seed, n){
  if(iter == 1){
    results <- list(iter = iter, fixef = fixef(model))
  } else {
    df       <- data.frame(x = rnorm(n), y = rnorm(n))
    df$xlow  <- ifelse(df$x <= br, df$x - br, 0)
    df$xhigh <- ifelse(df$x > br, df$x - br, 0)     
    df$high  <- ifelse(df$x > br, 1, 0)
    results <- list(iter = iter, fixef = fixef(update(model,
                                                      newdata = df,
                                                      recompile = FALSE,
                                                      silent = TRUE,
                                                      seed = seed)))
    
  }
  return(results)
}

# /*
# ----------------------------- Create rslurm job --------------------------
# */
# Job parameters
n_nodes       <- 4
cpus_per_node <- 24

sjob <- slurm_apply(twolineSim, pars, jobname = 'twoline_onePoint_sim3',
                    add_objects = c("model", 'br', 'nBr'),
                    nodes = n_nodes, cpus_per_node = cpus_per_node, submit = FALSE)