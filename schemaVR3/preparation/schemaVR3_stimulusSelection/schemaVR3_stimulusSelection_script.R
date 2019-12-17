###############################################################
# Functions
convertSeconds <- function(seconds){
  seconds <- round(seconds)
  if(seconds < 60){
    # Less than a minute
    time    <- paste(round(seconds), 'sec')
  } 
  if(seconds >= 60 & seconds < 3600){
    # Less than an hour
    minutes <- (seconds - seconds %% 60)/60
    seconds <- seconds %% 60
    time    <- paste(minutes, 'min,', round(seconds), 'sec')
  } 
  if(seconds >= 3600 & seconds < 86400){
    # Less than a day
    hours   <-  (seconds - seconds %% 3600)/3600
    minutes <-  ((seconds %% 3600) - (seconds %% 3600) %% 60)/60
    seconds <-  (seconds %% 3600) %% 60
    time    <- paste(hours, 'h,', minutes, 'min,', round(seconds), 'sec')
  } 
  if(seconds >= 86400){
    # More than a day
    days    <- (seconds - seconds %% 86400)/86400
    hours   <- ((seconds %% 86400) - (seconds %% 86400) %% 3600)/3600
    minutes <- (((seconds %% 86400) %% 3600) - ((seconds %% 86400) %% 3600) %% 60)/60
    seconds <- ((seconds %% 86400) %% 3600) %% 60
    time    <- paste(days, 'd, ', hours, 'h,', minutes, 'min,', round(seconds), 'sec')
  }
  return(time)
}

progressDisplay <- function(i, iterations, startTime){
  startTime     <- as.numeric(startTime)
  currentTime   <- as.numeric(Sys.time())
  elapsedTime   <- currentTime - startTime
  predictedTime <- elapsedTime * (1/(i/iterations))
  cat('\rProgress: |',rep('=',floor((i/iterations)*50)),rep(' ',50 - floor((i/iterations)*50)),'|',
      '\tElapsed time:', convertSeconds(elapsedTime),
      '\tTotal time:',    convertSeconds(predictedTime), '\t\t',
      sep = '')
}

###############################################################
# Loading data
# Preparing
subNo                <- 1:6
N                    <- length(subNo)
numberObjects        <- 20
kitchenOjects        <- 1:12
numberKitchenObjects <- length(kitchenOjects)
nonKitchenObjets     <- 13:30
locationRatings      <- array(data = NA, dim = c(numberObjects, numberObjects, N))

# Sequently loading data
for(i in 1:N){
  locationRatings[,,i] <- matrix(scan(paste('/home/aq01/Projects/schemaVR/schemaVR3/preparation/schemaVR2_stimulusSelection/data/locationRatings_', as.character(subNo[i]) ,'.dat', sep = '')), byrow = TRUE, ncol = 20)
}

# Calculating ranks
rankedRatings <- array(data = NA, dim = c(numberObjects, numberObjects, N))

for(i in 1:N){
  rankedRatings[,,i] <- rank(locationRatings[,,i])
}

# Calculating mean rank per object/ location
meanRankedRatings <- apply(rankedRatings, 1:2, mean)


###############################################################
# Calculating sum of squares
# Setting up variables
iterations <- 10000000
SS_target  <- rep(NA, iterations)
targets    <- matrix(NA, nrow = numberObjects, ncol = iterations)

# Calculating the targetspread of the 12 kitchen objects
targetSpread <- seq(1, 400,length = numberKitchenObjects)

# Main loop
for(i in 1:iterations){
  if(i == 1){
    startTime <- Sys.time()
  } 
  progressDisplay(i, iterations, startTime)
  targets[, i] <- sample(numberObjects)
  
  # Calculating target SS based only on the 12 kitchen objects
  kitchenTargets <- targets[seq(kitchenOjects), i]
  SS_target[i]   <- sum((sort(meanRankedRatings[cbind(seq_along(kitchenTargets),  kitchenTargets)]) - targetSpread)^2)
}

###############################################################
# Save data
save.image(paste('schemaVR3_selectionData_',format(Sys.time(), "%Y%m%d_%H%M"), '.RData', sep = ""))