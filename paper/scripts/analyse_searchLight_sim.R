# /*
# ----------------------------- General stuff --------------------------
# */
# Remove all
rm(list = ls(all.names = TRUE))

# Libraries
library(brms)
library(rslurm)
library(plyr)

# Set wd
setwd("U:/Projects/schemaVR/schemaVR_paper/scripts")

# Load data
batch0 <- readRDS('_rslurm_twoline_searchLight_sim2/results_0.RDS', refhook = NULL)
batch1 <- readRDS('_rslurm_twoline_searchLight_sim2/results_1.RDS', refhook = NULL)
batch2 <- readRDS('_rslurm_twoline_searchLight_sim2/results_2.RDS', refhook = NULL)
batch3 <- readRDS('_rslurm_twoline_searchLight_sim2/results_3.RDS', refhook = NULL)

# /*
# ----------------------------- batch0 --------------------------
# */
batchLength <- 2500
nBr         <- 17
for(i in 1:batchLength){
  # Loop for each iteration
  for(j in 1:nBr){
    # Loop for each breaking point
    
    # Reset values
    uShape     <- 0
    lowEffect  <- 0
    highEffect <- 0
    
    # Check for CI that are not overlaping with zero 
    if(batch0[[i]][[j]]$fixef[2, 4] < 0 & batch0[[i]][[j]]$fixef[3, 3] > 0){
      uShape <- 1
    } 
    if(batch0[[i]][[j]]$fixef[2, 4] < 0 ){
      lowEffect <- 1
    }
    if(batch0[[i]][[j]]$fixef[3, 3] > 0){
      highEffect <- 1
    }
    
    # Add to df
    if(!exists("df0")){
      df0 <- data.frame(iter = batch0[[i]][[j]]$iter,
                        br = batch0[[i]][[j]]$br,
                        uShape = uShape,
                        lowEffect = lowEffect, 
                        highEffect = highEffect,
                        xlow = batch0[[i]][[j]]$fixef[2,1],
                        xlow.se = batch0[[i]][[j]]$fixef[2,2],
                        xlow.q2.5 = batch0[[i]][[j]]$fixef[2,3],
                        xlow.q97.5 = batch0[[i]][[j]]$fixef[2,4],
                        xhigh = batch0[[i]][[j]]$fixef[3,1],
                        xhigh.se = batch0[[i]][[j]]$fixef[3,2],
                        xhigh.q2.5 = batch0[[i]][[j]]$fixef[3,3],
                        xhigh.q97.5 = batch0[[i]][[j]]$fixef[3,4])
    } else {
      df0 <- rbind(df0, data.frame(iter = batch0[[i]][[j]]$iter,
                                   br = batch0[[i]][[j]]$br,
                                   uShape = uShape,
                                   lowEffect = lowEffect, 
                                   highEffect = highEffect,
                                   xlow = batch0[[i]][[j]]$fixef[2,1],
                                   xlow.se = batch0[[i]][[j]]$fixef[2,2],
                                   xlow.q2.5 = batch0[[i]][[j]]$fixef[2,3],
                                   xlow.q97.5 = batch0[[i]][[j]]$fixef[2,4],
                                   xhigh = batch0[[i]][[j]]$fixef[3,1],
                                   xhigh.se = batch0[[i]][[j]]$fixef[3,2],
                                   xhigh.q2.5 = batch0[[i]][[j]]$fixef[3,3],
                                   xhigh.q97.5 = batch0[[i]][[j]]$fixef[3,4]))
    }
  }
}


# /*
# ----------------------------- batch1 --------------------------
# */
for(i in 1:batchLength){
  # Loop for each iteration
  for(j in 1:nBr){
    # Loop for each breaking point
    
    # Reset values
    uShape     <- 0
    lowEffect  <- 0
    highEffect <- 0
    
    # Check for CI that are not overlaping with zero 
    if(batch1[[i]][[j]]$fixef[2, 4] < 0 & batch1[[i]][[j]]$fixef[3, 3] > 0){
      uShape <- 1
    } 
    if(batch1[[i]][[j]]$fixef[2, 4] < 0 ){
      lowEffect <- 1
    }
    if(batch1[[i]][[j]]$fixef[3, 3] > 0){
      highEffect <- 1
    }
    
    # Add to df
    if(!exists("df1")){
      df1 <- data.frame(iter = batch1[[i]][[j]]$iter,
                        br = batch1[[i]][[j]]$br,
                        uShape = uShape,
                        lowEffect = lowEffect, 
                        highEffect = highEffect,
                        xlow = batch1[[i]][[j]]$fixef[2,1],
                        xlow.se = batch1[[i]][[j]]$fixef[2,2],
                        xlow.q2.5 = batch1[[i]][[j]]$fixef[2,3],
                        xlow.q97.5 = batch1[[i]][[j]]$fixef[2,4],
                        xhigh = batch1[[i]][[j]]$fixef[3,1],
                        xhigh.se = batch1[[i]][[j]]$fixef[3,2],
                        xhigh.q2.5 = batch1[[i]][[j]]$fixef[3,3],
                        xhigh.q97.5 = batch1[[i]][[j]]$fixef[3,4])
    } else {
      df1 <- rbind(df1, data.frame(iter = batch1[[i]][[j]]$iter,
                                   br = batch1[[i]][[j]]$br,
                                   uShape = uShape,
                                   lowEffect = lowEffect, 
                                   highEffect = highEffect,
                                   xlow = batch1[[i]][[j]]$fixef[2,1],
                                   xlow.se = batch1[[i]][[j]]$fixef[2,2],
                                   xlow.q2.5 = batch1[[i]][[j]]$fixef[2,3],
                                   xlow.q97.5 = batch1[[i]][[j]]$fixef[2,4],
                                   xhigh = batch1[[i]][[j]]$fixef[3,1],
                                   xhigh.se = batch1[[i]][[j]]$fixef[3,2],
                                   xhigh.q2.5 = batch1[[i]][[j]]$fixef[3,3],
                                   xhigh.q97.5 = batch1[[i]][[j]]$fixef[3,4]))
    }
  }
}

# /*
# ----------------------------- batch2 --------------------------
# */
for(i in 1:batchLength){
  # Loop for each iteration
  for(j in 1:nBr){
    # Loop for each breaking point
    
    # Reset values
    uShape     <- 0
    lowEffect  <- 0
    highEffect <- 0
    
    # Check for CI that are not overlaping with zero 
    if(batch2[[i]][[j]]$fixef[2, 4] < 0 & batch2[[i]][[j]]$fixef[3, 3] > 0){
      uShape <- 1
    } 
    if(batch2[[i]][[j]]$fixef[2, 4] < 0 ){
      lowEffect <- 1
    }
    if(batch2[[i]][[j]]$fixef[3, 3] > 0){
      highEffect <- 1
    }
    
    # Add to df
    if(!exists("df2")){
      df2 <- data.frame(iter = batch2[[i]][[j]]$iter,
                        br = batch2[[i]][[j]]$br,
                        uShape = uShape,
                        lowEffect = lowEffect, 
                        highEffect = highEffect,
                        xlow = batch2[[i]][[j]]$fixef[2,1],
                        xlow.se = batch2[[i]][[j]]$fixef[2,2],
                        xlow.q2.5 = batch2[[i]][[j]]$fixef[2,3],
                        xlow.q97.5 = batch2[[i]][[j]]$fixef[2,4],
                        xhigh = batch2[[i]][[j]]$fixef[3,1],
                        xhigh.se = batch2[[i]][[j]]$fixef[3,2],
                        xhigh.q2.5 = batch2[[i]][[j]]$fixef[3,3],
                        xhigh.q97.5 = batch2[[i]][[j]]$fixef[3,4])
    } else {
      df2 <- rbind(df2, data.frame(iter = batch2[[i]][[j]]$iter,
                                   br = batch2[[i]][[j]]$br,
                                   uShape = uShape,
                                   lowEffect = lowEffect, 
                                   highEffect = highEffect,
                                   xlow = batch2[[i]][[j]]$fixef[2,1],
                                   xlow.se = batch2[[i]][[j]]$fixef[2,2],
                                   xlow.q2.5 = batch2[[i]][[j]]$fixef[2,3],
                                   xlow.q97.5 = batch2[[i]][[j]]$fixef[2,4],
                                   xhigh = batch2[[i]][[j]]$fixef[3,1],
                                   xhigh.se = batch2[[i]][[j]]$fixef[3,2],
                                   xhigh.q2.5 = batch2[[i]][[j]]$fixef[3,3],
                                   xhigh.q97.5 = batch2[[i]][[j]]$fixef[3,4]))
    }
  }
}

# /*
# ----------------------------- batch3 --------------------------
# */
for(i in 1:batchLength){
  # Loop for each iteration
  for(j in 1:nBr){
    # Loop for each breaking point
    
    # Reset values
    uShape     <- 0
    lowEffect  <- 0
    highEffect <- 0
    
    # Check for CI that are not overlaping with zero 
    if(batch3[[i]][[j]]$fixef[2, 4] < 0 & batch3[[i]][[j]]$fixef[3, 3] > 0){
      uShape <- 1
    } 
    if(batch3[[i]][[j]]$fixef[2, 4] < 0 ){
      lowEffect <- 1
    }
    if(batch3[[i]][[j]]$fixef[3, 3] > 0){
      highEffect <- 1
    }
    
    # Add to df
    if(!exists("df3")){
      df3 <- data.frame(iter = batch3[[i]][[j]]$iter,
                        br = batch3[[i]][[j]]$br,
                        uShape = uShape,
                        lowEffect = lowEffect, 
                        highEffect = highEffect,
                        xlow = batch3[[i]][[j]]$fixef[2,1],
                        xlow.se = batch3[[i]][[j]]$fixef[2,2],
                        xlow.q2.5 = batch3[[i]][[j]]$fixef[2,3],
                        xlow.q97.5 = batch3[[i]][[j]]$fixef[2,4],
                        xhigh = batch3[[i]][[j]]$fixef[3,1],
                        xhigh.se = batch3[[i]][[j]]$fixef[3,2],
                        xhigh.q2.5 = batch3[[i]][[j]]$fixef[3,3],
                        xhigh.q97.5 = batch3[[i]][[j]]$fixef[3,4])
    } else {
      df3 <- rbind(df3, data.frame(iter = batch3[[i]][[j]]$iter,
                                   br = batch3[[i]][[j]]$br,
                                   uShape = uShape,
                                   lowEffect = lowEffect, 
                                   highEffect = highEffect,
                                   xlow = batch3[[i]][[j]]$fixef[2,1],
                                   xlow.se = batch3[[i]][[j]]$fixef[2,2],
                                   xlow.q2.5 = batch3[[i]][[j]]$fixef[2,3],
                                   xlow.q97.5 = batch3[[i]][[j]]$fixef[2,4],
                                   xhigh = batch3[[i]][[j]]$fixef[3,1],
                                   xhigh.se = batch3[[i]][[j]]$fixef[3,2],
                                   xhigh.q2.5 = batch3[[i]][[j]]$fixef[3,3],
                                   xhigh.q97.5 = batch3[[i]][[j]]$fixef[3,4]))
    }
  }
}

# /*
# ----------------------------- Create 1 df and summarise --------------------------
# */
df_all <- rbind(df0, df1, df2, df3)
df_agg <- ddply(df_all, 
                c('iter'), 
                summarise, 
                uShape = sum(uShape),
                lowEffect = sum(lowEffect),
                highEffect = sum(highEffect))

# /*
# ----------------------------- Analyse results --------------------------
# */
false_uShapes <- df_all[which(df_all$uShape == 1), ]

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
save(false_uShapes, df_agg, file = 'searchLight_sim_results2.RData')
