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
batch0 <- readRDS('_rslurm_twoline_sim2/results_0.RDS', refhook = NULL)
batch1 <- readRDS('_rslurm_twoline_sim2/results_1.RDS', refhook = NULL)
batch2 <- readRDS('_rslurm_twoline_sim2/results_2.RDS', refhook = NULL)
batch3 <- readRDS('_rslurm_twoline_sim2/results_3.RDS', refhook = NULL)

# /*
# ----------------------------- batch0 --------------------------
# */
batchLength <- 250
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
                        highEffect = highEffect)
    } else {
      df0 <- rbind(df0, data.frame(iter = batch0[[i]][[j]]$iter,
                                   br = batch0[[i]][[j]]$br,
                                   uShape = uShape,
                                   lowEffect = lowEffect, 
                                   highEffect = highEffect))
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
                        highEffect = highEffect)
    } else {
      df1 <- rbind(df1, data.frame(iter = batch1[[i]][[j]]$iter,
                                   br = batch1[[i]][[j]]$br,
                                   uShape = uShape,
                                   lowEffect = lowEffect, 
                                   highEffect = highEffect))
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
                        highEffect = highEffect)
    } else {
      df2 <- rbind(df2, data.frame(iter = batch2[[i]][[j]]$iter,
                                   br = batch2[[i]][[j]]$br,
                                   uShape = uShape,
                                   lowEffect = lowEffect, 
                                   highEffect = highEffect))
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
                        highEffect = highEffect)
    } else {
      df3 <- rbind(df3, data.frame(iter = batch3[[i]][[j]]$iter,
                                   br = batch3[[i]][[j]]$br,
                                   uShape = uShape,
                                   lowEffect = lowEffect, 
                                   highEffect = highEffect))
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

sum(df_agg$uShape != 0)
sum(df_agg$lowEffect != 0)
sum(df_agg$highEffect != 0)

table(df_agg$uShape)
table(df_agg$lowEffect)
table(df_agg$highEffect)

# Calculate BF10
# indices <- which(uShape == 1)
# dnorm(0)/dnorm(0, results[[indices[1]]][2,1], results[[indices[1]]][2,2])
# dnorm(0)/dnorm(0, results[[indices[2]]][2,1], results[[indices[2]]][2,2])
# dnorm(0)/dnorm(0, results[[indices[3]]][2,1], results[[indices[3]]][2,2])
# dnorm(0)/dnorm(0, results[[indices[4]]][2,1], results[[indices[4]]][2,2])