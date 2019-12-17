# /*
# ----------------------------- General stuff --------------------------
# */
# Remove df0 if exists
rm('df0')

# Libraries
library(brms)
library(rslurm)
library(plyr)

# Set wd
setwd("U:/Projects/schemaVR/schemaVR_paper/scripts")

# Load data
batch0 = readRDS('_rslurm_twoline_onePoint_sim3/results_0.RDS', refhook = NULL)
batch1 = readRDS('_rslurm_twoline_onePoint_sim3/results_1.RDS', refhook = NULL)
batch2 = readRDS('_rslurm_twoline_onePoint_sim3/results_2.RDS', refhook = NULL)
batch3 = readRDS('_rslurm_twoline_onePoint_sim3/results_3.RDS', refhook = NULL)

# /*
# ----------------------------- batches --------------------------
# */
batches     <- 0:3
batchLength <- 2500
for(i in batches){
  tempBatch <- get(paste('batch', i, sep = ''))
  for(j in 1:batchLength){
    # Reset values
    uShape     <- 0
    lowEffect  <- 0
    highEffect <- 0
    
    # Check for CI that are not overlaping with zero 
    if(tempBatch[[j]]$fixef[2, 4] < 0 & tempBatch[[j]]$fixef[3, 3] > 0){
      uShape <- 1
    } 
    if(tempBatch[[j]]$fixef[2, 4] < 0 ){
      lowEffect <- 1
    }
    if(tempBatch[[j]]$fixef[3, 3] > 0){
      highEffect <- 1
    }
    
    # Add to df
    if(!exists("df0")){
      df0 <- data.frame(iter = tempBatch[[j]]$iter,
                        uShape = uShape,
                        lowEffect = lowEffect, 
                        highEffect = highEffect,
                        xlow = tempBatch[[j]]$fixef[2,1],
                        xlow.se = tempBatch[[j]]$fixef[2,2],
                        xlow.q2.5 = tempBatch[[j]]$fixef[2,3],
                        xlow.q97.5 = tempBatch[[j]]$fixef[2,4],
                        xhigh = tempBatch[[j]]$fixef[3,1],
                        xhigh.se = tempBatch[[j]]$fixef[3,2],
                        xhigh.q2.5 = tempBatch[[j]]$fixef[3,3],
                        xhigh.q97.5 = tempBatch[[j]]$fixef[3,4])
    } else {
      df0 <- rbind(df0, data.frame(iter = tempBatch[[j]]$iter,
                                   uShape = uShape,
                                   lowEffect = lowEffect, 
                                   highEffect = highEffect,
                                   xlow = tempBatch[[j]]$fixef[2,1],
                                   xlow.se = tempBatch[[j]]$fixef[2,2],
                                   xlow.q2.5 = tempBatch[[j]]$fixef[2,3],
                                   xlow.q97.5 = tempBatch[[j]]$fixef[2,4],
                                   xhigh = tempBatch[[j]]$fixef[3,1],
                                   xhigh.se = tempBatch[[j]]$fixef[3,2],
                                   xhigh.q2.5 = tempBatch[[j]]$fixef[3,3],
                                   xhigh.q97.5 = tempBatch[[j]]$fixef[3,4]))
    }
  }
}


# /*
# ----------------------------- Analyse results --------------------------
# */
false_uShapes <- df0[which(df0$uShape == 1), ]

# calculate unrestrected BF10
false_uShapes$xlow_UR.BF10  <- dnorm(0, 0, 1)/dnorm(0, false_uShapes$xlow, false_uShapes$xlow.se)
false_uShapes$xhigh_UR.BF10 <- dnorm(0, 0, 1)/dnorm(0, false_uShapes$xhigh, false_uShapes$xhigh.se)


# Calculate order-restricted BF10
# Priors are the same for both as they as symmetrical
prior.OR <- dnorm(0, 0, 1)/0.5

false_uShapes$xlow_OR.BF10  <- NA
false_uShapes$xhigh_OR.BF10 <- NA
for(i in 1:dim(false_uShapes)[1]){
  # Low values
  areaPosterior   <- integrate(dnorm, 
                               mean = false_uShapes$xlow[i],
                               sd = false_uShapes$xlow.se[i], 
                               lower = -Inf, 
                               upper = 0, 
                               abs.tol = 0)$value 
  posterior.OR   <- dnorm(0, false_uShapes$xlow[i], false_uShapes$xlow.se[i])/areaPosterior
  false_uShapes$xlow_OR.BF10[i] <- prior.OR/posterior.OR
  
  # High values
  areaPosterior   <- integrate(dnorm, 
                               mean = false_uShapes$xhigh[i],
                               sd = false_uShapes$xhigh.se[i], 
                               lower = 0, 
                               upper = Inf, 
                               abs.tol = 0)$value 
  posterior.OR   <- dnorm(0, false_uShapes$xhigh[i], false_uShapes$xhigh.se[i])/areaPosterior
  false_uShapes$xhigh_OR.BF10[i] <- prior.OR/posterior.OR
}


# /*
# ----------------------------- Save results --------------------------
# */
save(false_uShapes, file = 'onePoint_sim_results.RData')