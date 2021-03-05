# This scripts create null data by shuffling the existing data
# Date: 01/09/2020
# Explanation: Since option 2 seems to be better, we know look for getting realistic null data. For this,
# I shuffle the existing data and run the same script on this. 

# /*
# ----------------------------- Libraries --------------------------
# */
library(plyr)
#library(rslurm)
library(dplyr)
library(lmerTest)
library(parallel)
library(doParallel)

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
setwd("~/Alex")

# Load data
load("nullData_input.RData")

# /*
# ----------------------------- Simulation--------------------------
# */
# Option 2 is to use all data and test a whole range of points within an intervall and then apply an approprriate correction for multiple testing.
# This script will examine the false positive rates and true positive rates to see which analysis is more suited. 

# Function
nullData_sim <- function(index, alphaLevel, br_range, numPoints){
  # Shuffle the data
  df <- ddply(raw_df, c('sub'), mutate, obj =  obj, s_x = s_x,  y = sample(y))
  
  # Possible breaking points that will be tested
  br_range_val   <- quantile(df$s_x, c((1 - br_range)/2, br_range + (1 - br_range)/2))
  breakingPoints <- seq(br_range_val[1], br_range_val[2], length.out =  numPoints)
  
  # Go through all breaking points
  x      <- df$s_x 
  for(i in 1:numPoints){
    # Interrupted regression at breaking point
    df$xlow  <- ifelse(x <= breakingPoints[i], x - breakingPoints[i], 0)
    df$xhigh <- ifelse(x > breakingPoints[i], x - breakingPoints[i], 0)     
    df$high  <- ifelse(x > breakingPoints[i], 1, 0)
    model    <- coef(summary(glmer(y ~ xlow + xhigh + high + (1 | sub) + (1 | obj), data = df, family = binomial)))
    
    # Check if all variables have values because sometime not all are defined because of singularities
    if(nrow(model) == 2){
      # Check which row names are included
      rowNames <- c('(Intercept)', 'xlow', 'xhigh', 'high')
      
      # Construct data frame with added NA for singularities
      model_new <- c()
      for(j in 1:length(rowNames)){
        if(rowNames[j] %in% rownames(model)){
          model_new <- rbind(model_new, model[which(rowNames[j] == rownames(model)),])
        } else {
          model_new <- rbind(model_new, rep(NA, 4))
        }
      }
      
      model <- model_new
    }
    
    # Add to df 
    if(i == 1){
      # Get results
      results <- data.frame(index          = index,
                            pointedTested  = i,
                            alphaLevel     = alphaLevel, 
                            br_range       = br_range,
                            numPoints      = numPoints,
                            breakingPoint  = breakingPoints[i],
                            uShaped        = model[2, 4] <= alphaLevel & model[3, 4] <= alphaLevel,
                            p1             = model[2, 4],
                            p2             = model[3, 4],
                            slope1         = model[2, 1],
                            slope2         = model[3, 1],
                            se1            = model[2, 2],
                            se2            = model[3, 2])
    } else {
      results <- rbind(results, data.frame(index          = index,
                                           pointedTested  = i,
                                           alphaLevel     = alphaLevel, 
                                           br_range       = br_range,
                                           numPoints      = numPoints,
                                           breakingPoint  = breakingPoints[i],
                                           uShaped        = model[2, 4] <= alphaLevel & model[3, 4] <= alphaLevel,
                                           p1             = model[2, 4],
                                           p2             = model[3, 4],
                                           slope1         = model[2, 1],
                                           slope2         = model[3, 1],
                                           se1            = model[2, 2],
                                           se2            = model[3, 2]))
    }
  }
  
  rownames(results) <- NULL
  return(results)
}

# Parameters of simulation
nIter      <- 10000
alphaLevel <- 0.05
br_range   <- 0.8 # middle % range of accept s_x values for breaking point
numPoints  <- 10

sim_params <- data.frame(alphaLevel = rep(alphaLevel, nIter),
                         br_range = rep(br_range, nIter),
                         numPoints = rep(numPoints,  nIter))

# Set up cores for parallel running
numCores <- detectCores(all.tests = FALSE, logical = TRUE)
cl       <- makePSOCKcluster(numCores - 1)
registerDoParallel(cl)

start1 <- Sys.time()
# Run sim 
sim <- foreach(i = 1:nIter, .packages = c('lmerTest', 'plyr'), .export = 'raw_df') %dopar% 
  {
    nullData_sim(i, sim_params$alphaLevel[i], sim_params$br_range[i], sim_params$numPoints[i])
  }
end1 <- Sys.time()

# stop cluster
stopCluster(cl)

# Bind to one data frame
sim_df <- bind_rows(sim)

# /*
# ----------------------------- Save image  --------------------------
# */
save.image('nullData_correction.RData')


# Print time
print('Time needed')
end1 - start1

# Shutdown after down
#system('shutdown -s')