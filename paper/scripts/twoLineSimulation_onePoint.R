# Script to simulate two line test with only one breaking point
# Version 1.0
# Date: 29/10/2019
# Author: Joern Alexander Quent
# /*
# ----------------------------- Libraries, settings and functions ---------------------------
# */
# Setting seed and cores
set.seed(9257)
cores2use <- 2

# Libraries
library(brms)
library(assortedRFunctions)

# Simulation parameters
nSims  <- 1000
n      <- 1142 # based on number of recall data 
br     <- 0
seeds  <- sample(9999999, nSims)
priors <- c(prior(normal(0, 1) , class = "Intercept"),
            prior(normal(0, 1) , class = "b", coef = 'high'),
            prior(normal(0, 1) , class = "b", coef = 'xlow'),
            prior(normal(0, 1)  , class = "b", coef = 'xhigh'))

results <- list()


# /*
# ----------------------------- Simulation --------------------------
# */
# First run
df       <- data.frame(x = rnorm(n), y = rnorm(n))
df$xlow  <- ifelse(df$x <= br, df$x - br, 0)
df$xhigh <- ifelse(df$x > br, df$x - br, 0)     
df$high  <- ifelse(df$x > br, 1, 0)
model    <- brm(y ~ xlow + xhigh + high,
                data = df,
                prior = priors,
                cores = cores2use,
                save_all_pars = TRUE,
                sample_prior = TRUE,
                seed = seeds[1])
results[[1]] <- fixef(model)

# Loop
for(i in 2:nSims){
  df       <- data.frame(x = rnorm(n), y = rnorm(n))
  df$xlow  <- ifelse(df$x <= br, df$x - br, 0)
  df$xhigh <- ifelse(df$x > br, df$x - br, 0)     
  df$high  <- ifelse(df$x > br, 1, 0)
  model    <- update(model,
                     newdata = df,
                     recompile = FALSE,
                     cores = cores2use,
                     save_all_pars = TRUE,
                     sample_prior = TRUE,
                     seed = seeds[i])
  results[[i]] <- fixef(model)
  cat(paste('Runs completed:', i, '\n'))
  
}

# /*
# ----------------------------- Saving image ---------------------------
# */
save.image(datedFileNam('twoLineSimulation_onePoint', '.RData'))