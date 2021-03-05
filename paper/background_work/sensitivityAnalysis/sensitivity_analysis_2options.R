# Sensitivity analysis of two options to analyse U-shape data
# Date: 25/08/2020
# Explanation: We identified two options to analyse the data. Option 1 is to split the data in half using a method to find a 
# minimum and use interrupted regression to test whether this both slopes at this point are different from 0. Option 2 is to 
# use all data and test a whole range of points within an interval and then apply an appropriate correction for multiple testing.
# This script will examine the false positive rates and true positive rates to see which analysis is more suited. 

# /*
# ----------------------------- Libraries --------------------------
# */
library(plyr)
library(mgcv) #This library has the additive model with smoothing function
#library(rslurm)
library(dplyr)
library(parallel)
library(doParallel)

# /*
# ----------------------------- Option 1 --------------------------
# */
# Option 1 is to find a minimum in half of the data and then run an interrupted regression in it. 
# This function finds the smooth minimum/maximum in a subset of the data (br_range). The alternative would be to fit it to whole dataset
# but only use br_range to restrict that range.  
find_min_max   <- function(df, func, br_range){
  # Subset to br_range
  cutOffs <- quantile(df$s_x, c((1 - br_range)/2, br_range + (1 - br_range)/2))
  df      <- df[df$s_x > cutOffs[1] & df$s_x < cutOffs[2], ]
  
  # Find smoothed min/max
  unique.x     <- length(unique(df$s_x))  # How many unique values x has
  form_spline  <- paste0("y ~ s(s_x, bs='cr', k = min(10,", unique.x,"))")
  smooth_model <- gam(as.formula(form_spline), data = df,  family = binomial)
  smooth_yhat  <- predict.gam(smooth_model)
  min_max_y    <- func(smooth_yhat)
  return(df$s_x[which(smooth_yhat == min_max_y)])
}

# I use the same data generator that I used to simulate the U-shape/linear paradox for remember judgements. That is because 
# shuffling the zeroes would only work for the null data.  
data_generator <- function(numObj, numSub, sdScale, beta0, beta1, beta2){
  # Give numObj obj true values from uniform distribution between -100 and 100.
  trueObj_val      <- runif(numObj, -100, 100)
 
  # Have an SD that scales with the true obj values
  sd_scaled_by_val <- sdScale*trueObj_val - sdScale*100
  
  # For each subject draw a random value with the mean of the object with sd that scales with this value
  subjVal <- matrix(NA, numSub, numObj)
  
  for(i in 1:numSub){
    for(j in 1:numObj){
      subjVal[i, j] <- rnorm(1, trueObj_val[j], sd_scaled_by_val[j])
    }
  }
  
  # Capping at -100 and 100
  subjVal[subjVal > 100]  <- 100
  subjVal[subjVal < -100] <- -100
  
  # Create DF and also scale x 
  df <- data.frame(sub  = rep(1:numSub, numObj),
                   obj  = rep(1:numObj, each = numSub),
                   x    = c(subjVal))
  
  # Scale
  df$s_x <- scale(df$x)
  
  # Predicting memory
  z     <- beta0 + beta1*df$s_x + beta2*df$s_x*df$s_x # Regression in log odds
  pr    <- 1/(1+exp(-z)) # Convert to probability.
  df$y  <- rbinom(numObj*numSub, 1, pr)
  return(df)
}

option1        <- function(numObj, numSub, sdScale, beta0, beta1, beta2, alphaLevel, br_range){
  # Simulate data
  df <- data_generator(numObj, numSub, sdScale, beta0, beta1, beta2)
  
  # Split in half
  subj        <- unique(df$sub)
  half1_index <- sample(subj, numSub/2) # Random selection of half
  half2_index <- subj[!subj %in% half1_index] #  number not included in half 1 are assigned to half 2
  half1       <- df[df$sub %in% half1_index,] 
  half2       <- df[df$sub %in% half2_index,]
  
  # Find min/max in half 1 in accepted range
  breakingPoint_h1  <- find_min_max(half1, min, br_range)[1]
  breakingPoint_h2  <- find_min_max(half2, min, br_range)[1]
  breakingPoint_all <- find_min_max(df, min, br_range)[1]
  
  # Interrupted regression with half 2 with min/max as breaking point
  x           <- half2$s_x 
  half2$xlow  <- ifelse(x <= breakingPoint_h1, x - breakingPoint_h1, 0)
  half2$xhigh <- ifelse(x > breakingPoint_h1, x - breakingPoint_h1, 0)     
  half2$high  <- ifelse(x > breakingPoint_h1, 1, 0)
  model       <- summary(glm(y ~ xlow + xhigh + high , data = half2, family = binomial))
  
  # Get results
  results <- data.frame(numObj = numObj,
                        numSub = numSub,
                        sdScale = sdScale,
                        beta0   = beta0,
                        beta1   = beta1,
                        beta2   = beta2,
                        alphaLevel = alphaLevel, 
                        br_range = br_range,
                        breakingPoint_h1  = breakingPoint_h1,
                        breakingPoint_h2  = breakingPoint_h2,
                        breakingPoint_all = breakingPoint_all,
                        uShaped           = model$coefficients[2, 4] <= alphaLevel & model$coefficients[3, 4] <= alphaLevel,
                        p1                = model$coefficients[2, 4],
                        p2                = model$coefficients[3, 4],
                        slope1            = model$coefficients[2, 1],
                        slope2            = model$coefficients[3, 1],
                        se1               = model$coefficients[2, 2],
                        se2               = model$coefficients[3, 2])
  return(results)
}

# Parameters of simulation
nIter      <- 10000
numBeta    <- 5
totalIter  <- nIter*numBeta
numObj     <- rep(20, numBeta) # Number of objects
numSub     <- rep(80, numBeta) # Number of subjects
sdScale    <- rep(-1, numBeta) # Scale the SD as a funicton of the x value
beta0      <- rep(0, numBeta)
beta1      <- rep(0.2, numBeta)
beta2      <- seq(0, 0.7, length.out =  numBeta)
alphaLevel <- rep(0.05, numBeta)
br_range   <- rep(0.8, numBeta) # middle % range of accept s_x values for breaking point

option1_params <- data.frame(numObj = rep(numObj, nIter),
                             numSub = rep(numSub, nIter),
                             sdScale = rep(sdScale, nIter),
                             beta0 = rep(beta0, nIter),
                             beta1 = rep(beta1, nIter),
                             beta2 = rep(beta2, nIter),
                             alphaLevel = rep(alphaLevel, nIter),
                             br_range = rep(br_range, nIter))

# Set up cores for parallel running
numCores <- detectCores(all.tests = FALSE, logical = TRUE)
cl       <- makePSOCKcluster(numCores - 1)
registerDoParallel(cl)


start1 <- Sys.time()
# Run sim 
sim <- foreach(i = 1:totalIter, .packages ='mgcv') %dopar% 
  {
    option1(option1_params$numObj[i], 
            option1_params$numSub[i], 
            option1_params$sdScale[i], 
            option1_params$beta0[i], 
            option1_params$beta1[i], 
            option1_params$beta2[i], 
            option1_params$alphaLevel[i], 
            option1_params$br_range[i])
  }
end1 <- Sys.time()

# stop cluster
stopCluster(cl)

# Save reults to data frame
sim_df_option1        <- bind_rows(sim)

# /*
# ----------------------------- Option 2 --------------------------
# */
# Option 2 is to use all data and test a whole range of points within an intervall and then apply an approprriate correction for multiple testing.
# This script will examine the false positive rates and true positive rates to see which analysis is more suited. 

# Function
option2 <- function(index, numObj, numSub, sdScale, beta0, beta1, beta2, alphaLevel, br_range, numPoints){
  # Simulate data
  df <- data_generator(numObj, numSub, sdScale, beta0, beta1, beta2)
  
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
    model    <- coef(summary(glm(y ~ xlow + xhigh + high , data = df, family = binomial)))
    
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
                            numObj         = numObj,
                            numSub         = numSub,
                            sdScale        = sdScale,
                            beta0          = beta0,
                            beta1          = beta1,
                            beta2          = beta2,
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
                                           numObj         = numObj,
                                           numSub         = numSub,
                                           sdScale        = sdScale,
                                           beta0          = beta0,
                                           beta1          = beta1,
                                           beta2          = beta2,
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
numBeta    <- 5
totalIter  <- nIter*numBeta
numObj     <- rep(20, numBeta) # Number of objects
numSub     <- rep(80, numBeta) # Number of subjects
sdScale    <- rep(-1, numBeta) # Scale the SD as a function of the x value
beta0      <- rep(0, numBeta)
beta1      <- rep(0.2, numBeta)
beta2      <- seq(0, 0.7, length.out =  numBeta)
alphaLevel <- rep(0.05, numBeta)
br_range   <- rep(0.8, numBeta) # middle % range of accept s_x values for breaking point
numPoints  <- rep(10, numBeta)

option2_params <- data.frame(numObj = rep(numObj, nIter),
                             numSub = rep(numSub, nIter),
                             sdScale = rep(sdScale, nIter),
                             beta0 = rep(beta0, nIter),
                             beta1 = rep(beta1, nIter),
                             beta2 = rep(beta2, nIter),
                             alphaLevel = rep(alphaLevel, nIter),
                             br_range = rep(br_range, nIter),
                             numPoints = rep(numPoints,  nIter))

# Set up cores for parallel running
numCores <- detectCores(all.tests = FALSE, logical = TRUE)
cl       <- makePSOCKcluster(numCores - 1)
registerDoParallel(cl)

start2 <- Sys.time()
# Run sim 
sim <- foreach(i = 1:totalIter, .packages ='mgcv') %dopar% 
  {
    option2(i, 
            option2_params$numObj[i], 
            option2_params$numSub[i], 
            option2_params$sdScale[i], 
            option2_params$beta0[i], 
            option2_params$beta1[i], 
            option2_params$beta2[i], 
            option2_params$alphaLevel[i], 
            option2_params$br_range[i],
            option2_params$numPoints[i])
  }
end2 <- Sys.time()

# stop cluster
stopCluster(cl)

# Bind to one data frame
sim_df_option2 <- bind_rows(sim)

# Collapse across runs
sim_df_option2_agg <- ddply(sim_df_option2, c('index', 'beta2'), summarise, uShaped = max(uShaped, na.rm = TRUE))

# /*
# ----------------------------- Save image  --------------------------
# */
save.image('sensitivity_analysis_2options.RData')


# Print time
print('Otion 1:')
end1 - start1

print('Otion 2:')
end2 - start2