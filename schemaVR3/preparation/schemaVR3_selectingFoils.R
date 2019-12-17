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

# Pre-selected sets
set.seed(422)
setNum <- c(848, 498, 388, 246, 111)
sets   <- read.csv("schemaVR3_sets.csv")

# Loading normative data
subNo                <- 1:6
N                    <- length(subNo)
numberObjects        <- 20
kitchenOjects        <- 1:12
numberKitchenObjects <- length(kitchenOjects)
nonKitchenObjets     <- 13:30
locationRatings      <- array(data = NA, dim = c(numberObjects, numberObjects, N))

# Sequently loading data
for(i in 1:N){
  locationRatings[,,i] <- matrix(scan(paste('/home/aq01/Projects/schemaVR/schemaVR3/preparation/schemaVR3_stimulusSelection/data/locationRatings_', as.character(subNo[i]) ,'.dat', sep = '')), byrow = TRUE, ncol = 20)
  #locationRatings[,,i] <- matrix(scan(paste('U:/Projects/schemaVR/schemaVR3/preparation/schemaVR3_stimulusSelection/data/locationRatings_', as.character(subNo[i]) ,'.dat', sep = '')), byrow = TRUE, ncol = 20)
}

# Calculating ranks
rankedRatings <- array(data = NA, 
                       dim = c(numberObjects, numberObjects, N))

for(i in 1:N){
  rankedRatings[,,i] <- rank(locationRatings[,,i])
}

# Calculating mean rank per object/ location
meanRankedRatings <- apply(rankedRatings, 1:2, mean)

iterations      <- 400000
numSets         <- length(setNum)
totalIterations <- iterations*numSets
foils1          <- array(data = NA, dim = c(numSets, iterations, numberObjects))
foils2          <- array(data = NA, dim = c(numSets, iterations, numberObjects))
SS_foils        <- array(data = NA, dim = c(numSets, iterations))

startTime <- Sys.time()
index     <- 0
for(i in 1: numSets){
  for(j in 1:iterations){
    foils1[i, j,]  <- sample(numberObjects)
    foils2[i, j,]  <- sample(numberObjects)
    while(any(foils1[i, j,]  == foils2[i, j,], sets[i + 1] == foils1[i, j,], sets[i + 1] == foils2[i, j,])){
      foils1[i, j,]  <- sample(numberObjects)
      foils2[i, j,]  <- sample(numberObjects)
    }
    SS_foils[i, j] <- sum((meanRankedRatings[cbind(seq_along(as.numeric(unlist(sets[i + 1]))),  as.numeric(unlist(sets[i + 1])))] - meanRankedRatings[cbind(seq_along(foils1[i, j,]),  foils1[i, j,])])^2, 
                          (meanRankedRatings[cbind(seq_along(as.numeric(unlist(sets[i + 1]))),  as.numeric(unlist(sets[i + 1])))] - meanRankedRatings[cbind(seq_along(foils2[i, j,]), foils2[i, j,])])^2)
    
    index <- index + 1
    progressDisplay(index, totalIterations, startTime)
  }
}

save.image(paste('data_selectingFoils_',format(Sys.time(), "%Y%m%d_%H%M"), '.RData', sep = ""))