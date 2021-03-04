# This scripts create null data by shuffling the existing data
# Date: 01/09/2020
# Explanation: Since option 2 seems to be better, we know look for getting realitic null data. For this ,
# I shuffle the existing data and run the same script on this. 

# Setting seed
set.seed(12154)

# /*
# ----------------------------- Libraries --------------------------
# */
# Set library path
.libPaths('/home/aq01/R/x86_64-redhat-linux-gnu-library/3.6')

library(plyr)
library(rslurm)
library(lmerTest)
#library(parallel)
#library(doParallel)

# /*
# ----------------------------- Load data --------------------------
# */
# This is the data that already has been analysed
# Loading data
# load("C:/Users/aq01/Desktop/schemaVR/schemaVR1/data/dataSchemaVR1_cleaned.RData") # DOes
# load("C:/Users/aq01/Desktop/schemaVR/schemaVR2/data/dataSchemaVR2_cleaned.RData")
# load('C:/Users/aq01/Desktop/schemaVR/schemaVR3/data/schemaVR3_closestLocation.RData')
# 
# # Make subNum character
# dataSchemaVR1$subNum <- as.character(dataSchemaVR1$subNum)
# dataSchemaVR2$subNum <- as.character(dataSchemaVR2$subNum)
# dataSchemaVR3$subNum <- as.character(dataSchemaVR3$subNum)
# 
# # Combine datasets
# combinedData <- data.frame(subNum = c(dataSchemaVR1$subNum, dataSchemaVR2$subNum, dataSchemaVR3$subNum),
#                            objNum = c(dataSchemaVR1$subNum, dataSchemaVR2$objNum, dataSchemaVR3$objNum),
#                            objNam = c(as.character(dataSchemaVR1$objName), as.character(dataSchemaVR2$objName), as.character(dataSchemaVR3$objNam)),
#                            targetLocation = c(dataSchemaVR1$targetLocation, dataSchemaVR2$targetLocation, dataSchemaVR3$targetLocation),
#                            objLocTargetRating = c(dataSchemaVR1$objLocTargetRating, dataSchemaVR2$objLocTargetRating, dataSchemaVR3$objLocTargetRating),
#                            accRecall = c(dataSchemaVR1$accRecall, dataSchemaVR2$accRecall, dataSchemaVR3$accRecall),
#                            accAFC  = c(dataSchemaVR1$accAFC, dataSchemaVR2$accAFC, dataSchemaVR3$accAFC))
# 
# combinedData$s_Exp <- scale(combinedData$objLocTargetRating)
# 
# # Create DF and also scale x 
# raw_df <- data.frame(sub  = combinedData$subNum,
#                      obj  = combinedData$objNam,
#                      s_x  = combinedData$s_Exp,
#                      y    = combinedData$accAFC)

# Set wd
#setwd("~/Alex")

# Load data
load("nullData_input.RData")

# /*
# ----------------------------- Simulation--------------------------
# */
# Option 2 is to use all data and test a whole range of points within an intervall and then apply an approprriate correction for multiple testing.
# This script will examine the false positive rates and true positive rates to see which analysis is more suited. 

# Function
nullData_sim <- function(index, br_range, numPoints){
  # Shuffle the data
  df <- ddply(raw_df, c('sub'), mutate, obj =  obj, s_x = s_x,  y = sample(y))
  
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
    
    # Using tryCatch because sometimes the model rethrows an error
    model1      <- tryCatch({
      model1      <- glmer(y ~ xlow + xhigh + high + (1 | sub), data = df, family = binomial)
    }, error = function(e){
      return(as.character(e))
    })
    
    # Check if model or error
    if(class(model1)[1] == "glmerMod"){
      model1_sum  <- summary(model1)
      
      # Extract convergence
      cc1         <- model1_sum$optinfo$conv$opt
      msgs1       <- paste(unlist(model1_sum$optinfo$conv$lme4$messages), collapse = " ")
      nmsgs1      <- length(msgs1)
      if(nmsgs1 == 0){
        msgs1 <- NA_character_
      }
      warnings1  <- paste(unlist(model1_sum$optinfo$warnings), collapse = " ")
      nwarnings1 <- length(warnings1)
      if(nwarnings1 == 0){
        warnings1 <- NA_character_
      }
      
      # Extract coefficients
      model1    <- coef(model1_sum)
      p1        <- model1[2, 4]
      slope1    <- model1[2, 1]
      se1       <- model1[2, 2]
    } else {
      p1        <- NA
      slope1    <- NA
      se1       <- NA
      cc1       <- NA
      warnings1 <- NA
      msgs1     <- model1
    }
    
    # Slope 2 (breaking point is included in the second line)
    df$xlow     <- ifelse(x < breakingPoints[i], x - breakingPoints[i], 0)
    df$xhigh    <- ifelse(x >= breakingPoints[i], x - breakingPoints[i], 0)     
    df$high     <- ifelse(x >= breakingPoints[i], 1, 0)
    
    # Using tryCatch because sometimes the model rethrows an error
    model2      <- tryCatch({
      model2      <- glmer(y ~ xlow + xhigh + high  + (1 | sub), data = df, family = binomial)
    }, error = function(e){
      return(as.character(e))
    })
    
    # Check if model or error
    if(class(model2)[1] == "glmerMod"){
      model2_sum  <- summary(model2)
      
      # Extract convergence
      cc2         <- model2_sum$optinfo$conv$opt
      msgs2       <- paste(unlist(model2_sum$optinfo$conv$lme4$messages), collapse = " ")
      nmsgs2      <- length(msgs2)
      if(nmsgs2 == 0){
        msgs2 <- NA_character_
      }
      warnings2  <- paste(unlist(model2_sum$optinfo$warnings), collapse = " ")
      nwarnings2 <- length(warnings2)
      if(nwarnings2 == 0){
        warnings2 <- NA_character_
      }
      
      # Extract coefficients
      model2    <- coef(model2_sum)
      p2        <- model2[3, 4]
      slope2    <- model2[3, 1]
      se2       <- model2[3, 2]
    } else {
      p2        <- NA
      slope2    <- NA
      se2       <- NA
      cc2       <- NA
      warnings2 <- NA
      msgs2     <- model2
    }
    
    # Bind to one DF
    tempRessults <- data.frame(index          = index,
                               pointedTested  = i,
                               br_range       = br_range,
                               numPoints      = numPoints,
                               breakingPoint  = breakingPoints[i],
                               p1             = p1,
                               p2             = p2,
                               slope1         = slope1,
                               slope2         = slope2,
                               se1            = se1,
                               se2            = se2,
                               cc1            = cc1,
                               cc2            = cc2,
                               msgs1          = msgs1,
                               msgs2          = msgs2,
                               warnings1      = warnings1,
                               warnings2      = warnings2)
    
    
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
n_nodes       <- 4
cpus_per_node <- 20

# Parameters of simulation
nIter      <- 100000
br_range   <- 0.8 # middle % range of accept s_x values for breaking point
numPoints  <- 10

# Parameters
sim_params <- data.frame(index = 1:nIter,
                         br_range = rep(br_range, nIter),
                         numPoints = rep(numPoints,  nIter))

# Create job
sjob1 <- slurm_apply(nullData_sim, sim_params, jobname = 'nullData_sim',
                     add_objects = c('raw_df'),
                     nodes = n_nodes, cpus_per_node = cpus_per_node, submit = FALSE)