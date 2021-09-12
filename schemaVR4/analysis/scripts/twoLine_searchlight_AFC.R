# This scripts run a two line searchlight analysis to test for U-shapedness for AFC data
# Date: 12/07/2020
# Version 2.0
# Explanation: It's in the title :)
# Ask to remove everything in the global environment
#assortedRFunctions::clear_environment()

# Setting seed
set.seed(244)

######################################################
# Path to parent folder schemaVR
path2parent <- "D:/Alex/Laptop/Desktop/schemaVR" # This need to be changed to run this document
######################################################



# Setting WD
setwd(paste0(path2parent, "/schemaVR4/analysis"))

# /*
# ----------------------------- Libraries --------------------------
# */
library(brms)
library(rslurm)
library(polspline)
library(assortedRFunctions)

# /*
# ----------------------------- Data --------------------------
# */
# Loading all .RData files
load(paste0(path2parent, "/schemaVR1/data/dataSchemaVR1_cleaned.RData"))
load(paste0(path2parent, "/schemaVR2/data/dataSchemaVR2_cleaned.RData"))
load(paste0(path2parent, "/schemaVR3/data/dataSchemaVR3_cleaned.RData"))
load(paste0(path2parent, "/schemaVR4/data/dataSchemaVR4_cleaned.RData"))

# Create AFC data for schemaVR4
dataSchemaVR4_AFC    <- subset(dataSchemaVR4, dataSchemaVR4$resCon != 0)

# Combine data to one data frame
combinedData_AFC <- data.frame(Experiment = c(rep('1', length(dataSchemaVR1_AFC$subNum)),
                                              rep('2', length(dataSchemaVR2_AFC$subNum)),
                                              rep('3', length(dataSchemaVR3_AFC$subNum)),
                                              rep('4', length(dataSchemaVR4_AFC$subNum))),
                               set        = c(rep('1', length(dataSchemaVR1_AFC$subNum)),
                                              rep('2', length(dataSchemaVR2_AFC$subNum)),
                                              as.character(dataSchemaVR3_AFC$setNum),
                                              as.character(dataSchemaVR4_AFC$setNum)),
                               subNum = c(as.character(dataSchemaVR1_AFC$subNum),
                                          as.character(dataSchemaVR2_AFC$subNum),
                                          as.character(dataSchemaVR3_AFC$subNum),
                                          as.character(dataSchemaVR4_AFC$subNum)),
                               objNum = c(dataSchemaVR1_AFC$objNum,
                                          dataSchemaVR2_AFC$objNum,
                                          dataSchemaVR3_AFC$objNum,
                                          dataSchemaVR4_AFC$objNum),
                               objLocTargetRating = c(dataSchemaVR1_AFC$objLocTargetRating,
                                                      dataSchemaVR2_AFC$objLocTargetRating,
                                                      dataSchemaVR3_AFC$objLocTargetRating,
                                                      dataSchemaVR4_AFC$objLocTargetRating),
                               accAFC = c(dataSchemaVR1_AFC$accAFC,
                                          dataSchemaVR2_AFC$accAFC,
                                          dataSchemaVR3_AFC$accAFC,
                                          dataSchemaVR4_AFC$accAFC))
# Create set names
combinedData_AFC$new_set <- as.factor(paste(combinedData_AFC$Experiment, combinedData_AFC$set, sep = '_'))

combinedData_AFC$Exp    <- combinedData_AFC$objLocTargetRating 
combinedData_AFC$subNum <- as.character(combinedData_AFC$subNum)

# Scaling based on Gelman et al. (2008) and https://github.com/stan-dev/stan/wiki/Prior-Choice-Recommendations
# Mean = 0 and SD = 0.5
combinedData_AFC$s_x <- (combinedData_AFC$Exp - mean(combinedData_AFC$Exp ))/(sd(combinedData_AFC$Exp)/0.5)
combinedData_AFC$y    <- combinedData_AFC$accAFC

# Assign to DF
df <- combinedData_AFC

library(ggplot2)
plt1 <- ggplot(combinedData_AFC , aes(x = objLocTargetRating, y = accAFC)) + 
  geom_jitter(width = 0, height = 0.1) + 
  geom_smooth(method = 'loess')

ggsave(datedFileNam('plot_AFC_1', '.png'), plot = plt1)

# /*
# ----------------------------- Functions --------------------------
# */
# Priors for all models
priors  <- c(prior(student_t(7, 0, 10) , class = "Intercept"),
             prior(student_t(7, 0, 1) , class = "b")) 

priorDensity <- dstudent_t(0, 7, 0, 1)


# Setting up DF for interrupted regression
breakingPoints <- 0 
x              <- df$s_x 
df$xlow        <- ifelse(x <= breakingPoints[1], x - breakingPoints[1], 0)
df$xhigh       <- ifelse(x > breakingPoints[1], x - breakingPoints[1], 0)     
df$high        <- ifelse(x > breakingPoints[1], 1, 0)

# Run 1 for model compilation
baseModel <- brm(y ~ xlow + xhigh + high  + 
                   new_set +
                   (1 | subNum) +
                   (1 | objNum),
                 data = df,
                 prior = priors,
                 family = bernoulli(),
                 chains = 1,
                 save_all_pars = TRUE,
                 sample_prior = TRUE, 
                 seed = 6353) 


twoLine_searchlight <- function(seed, df, br_range, numPoints){
  # Get startTime
  startTime <- Sys.time()
  
  # Set seed 
  set.seed(seed)
  seeds <- sample(.Machine$integer.max, numPoints)
  
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
    
    # Model 1
    error1      <- FALSE
    model1      <- tryCatch({
      model1 <- update(baseModel,
                       newdata = df,
                       recompile = FALSE,
                       cores = 1,
                       chains = 1,
                       iter = iterPerChain,
                       warmup = 3000,
                       save_all_pars = TRUE,
                       sample_prior = TRUE,
                       silent = TRUE,
                       refresh = 0,
                       seed = seeds[i])
    }, error = function(e){
      error1 <- TRUE
      return(as.character(e))
    })
    
    # Slope 2 (breaking point is included in the second line)
    df$xlow     <- ifelse(x < breakingPoints[i], x - breakingPoints[i], 0)
    df$xhigh    <- ifelse(x >= breakingPoints[i], x - breakingPoints[i], 0)     
    df$high     <- ifelse(x >= breakingPoints[i], 1, 0)
    
    # Model 2 
    error2      <- FALSE
    model2      <- tryCatch({
      model2 <- update(baseModel,
                       newdata = df,
                       recompile = FALSE,
                       cores = 1,
                       chains = 1,
                       iter = iterPerChain,
                       warmup = 3000,
                       save_all_pars = TRUE,
                       sample_prior = TRUE,
                       silent = TRUE,
                       refresh = 0,
                       seed = seeds[i])
    }, error = function(e){
      error2 <- TRUE
      return(as.character(e))
    })
    
    # Extract information
    # Slope 1
    if(!error1){
      # Extract fixef
      slope1   <- fixef(model1)[2, 1]
      q2.5_1   <- fixef(model1)[2, 3]
      q97.5_1  <- fixef(model1)[2, 4]
      
      # Calculate BF manually
      postDist       <- posterior_samples(model1)$b_xlow
      bf_1           <- savage_dickey_ratio(postDist, priorDensity, 0.5, 'two.sided')
      
      # Calculate OR BF manually 
      bf_OR_1       <- savage_dickey_ratio(postDist, priorDensity, 0.5, 'less')
      
      msg1 <- NA
    } else {
      slope1   <- NA
      q2.5_1   <- NA
      q97.5_1  <- NA
      bf_1     <- NA
      bf_OR_1  <- NA
      msg1     <- model1
    }

    # Slope 2
    # Extract fixef
    if(!error2){
      # Extract fixef
      slope2   <- fixef(model2)[3, 1]
      q2.5_2   <- fixef(model2)[3, 3]
      q97.5_2  <- fixef(model2)[3, 4]
      
      # Calculate BF manually
      postDist       <- posterior_samples(model2)$b_xhigh
      bf_2           <- savage_dickey_ratio(postDist, priorDensity, 0.5, 'two.sided')
      
      # Calculate OR BF manually 
      bf_OR_2        <- savage_dickey_ratio(postDist, priorDensity, 0.5, 'greater')
      
      msg2 <- NA
    } else {
      slope2   <- NA
      q2.5_2   <- NA
      q97.5_2  <- NA
      bf_2     <- NA
      bf_OR_2  <- NA
      msg2     <- model2
    }
    
    # Bind to one DF
    tempRessults <- data.frame(br_range       = br_range,
                               numPoints      = numPoints,
                               breakingPoint  = breakingPoints[i],
                               slope1         = slope1,
                               q2.5_1         = q2.5_1,
                               q97.5_1        = q97.5_1,
                               bf_1           = bf_1,
                               bf_OR_1        = bf_OR_1,
                               slope2         = slope2,
                               q2.5_2         = q2.5_2,
                               q97.5_2        = q97.5_2,
                               bf_2           = bf_2,
                               bf_OR_2        = bf_OR_2,
                               msg1           = msg1,
                               msg2           = msg2)
    
    # Show progress
    progressDisplay(i = i, iterations = numPoints, startTime)
    
    
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

iterPerChain  <- 9000

# Parameters of simulation
br_range   <- 0.8 # middle % range of accept s_x values for breaking point
numPoints  <- 10

# Run
results <- twoLine_searchlight(1241, df, br_range, 10)
results

# Libraries
library(MRColour)
library(reshape2)

# Melt to long 
results_long <- melt(results, id.vars=c("breakingPoint", "br_range", "numPoints"))

# Remove all unnecessary stuff
excluder     <- c('slope1', 'bf_1', 'bf_OR_1', 'slope2', 'bf_2', 'bf_OR_2', 'msg1', 'msg2')
results_long <- results_long[!(results_long$variable %in% excluder),]

# Rename variables
results_long$Slope <- NA
results_long$Slope[results_long$variable == 'q2.5_1']  <- 'Slope 1'
results_long$Slope[results_long$variable == 'q97.5_1'] <- 'Slope 1'
results_long$Slope[results_long$variable == 'q2.5_2']  <- 'Slope 2'
results_long$Slope[results_long$variable == 'q97.5_2'] <- 'Slope 2'

# Add lineID
results_long$lineId <- c(rep(1:nrow(results), 2), rep((nrow(results)+1):(nrow(results)*2), 2))

# Plot results
plt2<- ggplot(results_long, aes(x = value, y = breakingPoint, group = lineId, colour = Slope)) + 
  geom_point() + 
  geom_line(aes(group = lineId)) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  labs(title = '95% CI for two slopes', y = 'Breaking point') +
  scale_color_mrc()


ggsave(datedFileNam('plot_AFC_2', '.png'), plot = plt2)
# 
# 
# # /* 
# # ----------------------------- To full model ---------------------------
# # */
# # Cores to use
# cores2use <- 4
# 
# # Find middle between the two points that seems to show an effect
# middle_br <- mean(results$breakingPoint[3:4])
# 
# x      <- df$s_x 
# # Slope 1 (breaking point is included in the first line)
# df$xlow     <- ifelse(x <= middle_br, x - middle_br, 0)
# df$xhigh    <- ifelse(x > middle_br, x - middle_br, 0)     
# df$high     <- ifelse(x > middle_br, 1, 0)
# 
# # Model 1
# error1      <- FALSE
# model1      <- tryCatch({
#   model1 <- update(baseModel,
#                    newdata = df,
#                    recompile = FALSE,
#                    chains = 8,
#                    warmup = 2000,
#                    iter   = 16000,
#                    cores = cores2use,
#                    save_all_pars = TRUE,
#                    sample_prior = TRUE,
#                    save_dso = TRUE,
#                    silent = TRUE,
#                    refresh = 0,
#                    seed = 214)
# }, error = function(e){
#   error1 <- TRUE
#   return(as.character(e))
# })
# 
# # Slope 2 (breaking point is included in the second line)
# df$xlow     <- ifelse(x < middle_br, x - middle_br, 0)
# df$xhigh    <- ifelse(x >= middle_br, x - middle_br, 0)     
# df$high     <- ifelse(x >= middle_br, 1, 0)
# 
# # Model 2 
# error2      <- FALSE
# model2      <- tryCatch({
#   model2 <- update(baseModel,
#                    newdata = df,
#                    recompile = FALSE,
#                    chains = 8,
#                    warmup = 2000,
#                    iter   = 16000,
#                    cores = cores2use,
#                    save_all_pars = TRUE,
#                    sample_prior = TRUE,
#                    save_dso = TRUE,
#                    silent = TRUE,
#                    refresh = 0,
#                    seed = 214)
# }, error = function(e){
#   error2 <- TRUE
#   return(as.character(e))
# })
# 
# # Extract information
# # Slope 1
# if(!error1){
#   # Extract fixef
#   slope1   <- fixef(model1)[2, 1]
#   q2.5_1   <- fixef(model1)[2, 3]
#   q97.5_1  <- fixef(model1)[2, 4]
#   
#   # Calculate BF manually
#   postDist       <- posterior_samples(model1)$b_xlow
#   bf_1           <- savage_dickey_ratio(postDist, priorDensity, 0.5, 'two.sided')
#   
#   # Calculate OR BF manually 
#   bf_OR_1       <- savage_dickey_ratio(postDist, priorDensity, 0.5, 'less')
#   
#   msg1 <- NA
# } else {
#   slope1   <- NA
#   q2.5_1   <- NA
#   q97.5_1  <- NA
#   bf_1     <- NA
#   bf_OR_1  <- NA
#   msg1     <- model1
# }
# 
# # Slope 2
# # Extract fixef
# if(!error2){
#   # Extract fixef
#   slope2   <- fixef(model2)[3, 1]
#   q2.5_2   <- fixef(model2)[3, 3]
#   q97.5_2  <- fixef(model2)[3, 4]
#   
#   # Calculate BF manually
#   postDist       <- posterior_samples(model2)$b_xhigh
#   bf_2           <- savage_dickey_ratio(postDist, priorDensity, 0.5, 'two.sided')
#   
#   # Calculate OR BF manually 
#   bf_OR_2        <- savage_dickey_ratio(postDist, priorDensity, 0.5, 'greater')
#   
#   msg2 <- NA
# } else {
#   slope2   <- NA
#   q2.5_2   <- NA
#   q97.5_2  <- NA
#   bf_2     <- NA
#   bf_OR_2  <- NA
#   msg2     <- model2
# }
# 
# # Bind to one DF
# tempResults <- data.frame(br_range       = br_range,
#                           numPoints      = numPoints,
#                           breakingPoint  = middle_br,
#                           slope1         = slope1,
#                           q2.5_1         = q2.5_1,
#                           q97.5_1        = q97.5_1,
#                           bf_1           = bf_1,
#                           bf_OR_1        = bf_OR_1,
#                           slope2         = slope2,
#                           q2.5_2         = q2.5_2,
#                           q97.5_2        = q97.5_2,
#                           bf_2           = bf_2,
#                           bf_OR_2        = bf_OR_2,
#                           msg1           = msg1,
#                           msg2           = msg2)

# /* 
# ----------------------------- Saving results ---------------------------
# */
save.image(datedFileNam('schemaVR4_AFC_searchlight', '.RData'))