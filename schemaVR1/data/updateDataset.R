######################################################################################
# Update dataset for schemaVR1 
# This script is used to update the dataset for schemaVR1. 
# Version 1.0
# Date: 13/04/2018
# Author: Joern Alexander Quent
######################################################################################
# Setting path to my directory
setwd("U:/Projects/reports/exp1/analysis")
#setwd("~/GitHub/reports/exp1/analysis")

# In order to add new data, add participants' number to the list new.
new               <- c(16:17)
# Checking whether these entries already exist.
population        <- read.table('population.txt', header = TRUE)
acceptedEntries   <- new[!(new %in% population$subNum)]
if(length(acceptedEntries) == 0){
  stop('There are no new entries')
} else{
  # Updating population
  populationUpdated <- data.frame(subNum = c(population$subNum, acceptedEntries))
  # Saving changes to log file 
  write.table(populationUpdated, 
              file = 'population.txt', 
              col.names = TRUE, 
              row.names = FALSE, 
              quote = FALSE)
}
# acceptedEntries only contains those entries, which are not already part of the dataset.
#######################################################################################
# Libraries
library(data.table)
library(plyr)
library(car)

# Functions
euclideanDistance3D <-function(x1, y1, z1, x2, y2, z2){
  return(sqrt((x1 - x2)^2 + (y1 - y2)^2 + (z1 - z2)^2))
} # This function is used to calculate the Euclidean distance in 3D space

# General variables
objNames <- c('microwave',
              'kitchen roll',
              'saucepan', 
              'toaster',
              'bowl of fruits',
              'tea pot',
              'knife',
              'mixer',
              'bread',
              'glass jug',
              'mug',
              'dishes',
              'towels',
              'toy',
              'pile of books',
              'umbrella',
              'hat',
              'helmet',
              'calendar',
              'fan')

# Normative data and 3D locations
normativeData <- read.table('data/normativeData.txt', header = TRUE, sep = "\t")
spawnPoints2  <- read.table('data/spawnPoints2.txt', header = TRUE)

######################################################################################
# Processsing new data
########### Loading demographics.
demographics        <- rbindlist(lapply(paste('data/demographic_', as.character(acceptedEntries),'.txt', sep = ''), fread))
names(demographics) <- c('subNum',
                         'gender',
                         'age',
                         'date',
                         'startExp',
                         'endExp')

########### Loading data from location recall task.
dataLocationRecall        <- rbindlist(lapply(paste('data/outputFile_', as.character(acceptedEntries),'.txt', sep = ''), fread))
names(dataLocationRecall) <- c('subNum',
                               'trial',
                               'startTime',
                               'pickUpTime',
                               'dropDownTime',
                               'objName',
                               'objNum',
                               'respTime',
                               'answerTime',
                               'numAttempts',
                               'noMemory',
                               'xRetrieved',
                               'yRetrieved',
                               'zRetrieved')

# Loading encoding locations (i.e. correct locations).
encodingLocations        <- fread("data/inputFileEncoding.txt")
names(encodingLocations) <- c('objName', 
                              'xRotation', 
                              'yRotation', 
                              'zRotation', 
                              'xPosition', 
                              'yPosition', 
                              'zPosition')

# Adding correct locations to the dataset.
dataLocationRecall$objName          <- as.factor(dataLocationRecall$objName)
dataLocationRecall$xPositionCorrect <- 99
dataLocationRecall$yPositionCorrect <- 99
dataLocationRecall$zPositionCorrect <- 99
for(i in levels(dataLocationRecall$objName)){
  # Going through all objects and assign the correct location.
  dataLocationRecall[which(dataLocationRecall$objName == i)]$xPositionCorrect  <- encodingLocations[which(encodingLocations$objName == i)]$xPosition
  dataLocationRecall[which(dataLocationRecall$objName == i)]$yPositionCorrect  <- encodingLocations[which(encodingLocations$objName == i)]$yPosition
  dataLocationRecall[which(dataLocationRecall$objName == i)]$zPositionCorrect  <- encodingLocations[which(encodingLocations$objName == i)]$zPosition
}

# Calculating Euclidean distance between participants' placements and the correct location.
dataLocationRecall$euclideanDist <- euclideanDistance3D(dataLocationRecall$xRetrieved, 
                                                        dataLocationRecall$yRetrieved, 
                                                        dataLocationRecall$zRetrieved, 
                                                        dataLocationRecall$xPositionCorrect, 
                                                        dataLocationRecall$yPositionCorrect, 
                                                        dataLocationRecall$zPositionCorrect)

# Correcting objNames
dataLocationRecall[which(dataLocationRecall$objName == 'kitchenRoll')]$objName    <- 'kitchen roll'
dataLocationRecall[which(dataLocationRecall$objName == 'fruitBowl')]$objName      <- 'bowl of fruits'
dataLocationRecall[which(dataLocationRecall$objName == 'teaPot')]$objName         <- 'tea pot'
dataLocationRecall[which(dataLocationRecall$objName == 'glassContainer')]$objName <- 'glass jug'
dataLocationRecall[which(dataLocationRecall$objName == 'towel')]$objName          <- 'towels'
dataLocationRecall[which(dataLocationRecall$objName == 'bookPile')]$objName       <- 'pile of books'

########### Loading data from 3AFC task
dataAFC <- rbindlist(lapply(paste('data/retrievalTask_', as.character(acceptedEntries),'.dat', sep = ''), fread))
names(dataAFC) <- c('subNum', 
                    'date', 
                    'time', 
                    'trial',
                    'objNum', 
                    'targetLocation', 
                    'targetRankPre', 
                    'foil1Location', 
                    'foil1RankPre', 
                    'foil2Location', 
                    'foil2RankPre', 
                    'AFCPreTime',
                    'rtAFC', 
                    'resAFC', 
                    'accAFC', 
                    'conPreTime', 
                    'rtCon', 
                    'resCon', 
                    'left', 
                    'middle', 
                    'right')

# Assigning factor labels to variables
dataAFC$objName    <- factor(dataAFC$objNum, labels = objNames)
dataAFC$left       <- factor(dataAFC$left, labels = c('target', 'foil1', 'foil2'))
dataAFC$middle     <- factor(dataAFC$middle, labels = c('target', 'foil1', 'foil2'))
dataAFC$right      <- factor(dataAFC$left, labels = c('target', 'foil1', 'foil2'))
dataAFC$resCon     <- as.factor(dataAFC$resCon)

########### Loading data from object/location ratings (post-encoding).
dataRatingObjectLocation         <- rbindlist(lapply(paste('data/ratingObjectLocation_', as.character(acceptedEntries),'.dat', sep = ''), fread))
names(dataRatingObjectLocation)  <- c('subNum', 
                                      'date', 
                                      'time', 
                                      'trial',
                                      'objNum', 
                                      'location', 
                                      'rating', 
                                      'RT')
dataRatingObjectLocation$objName <- factor(dataRatingObjectLocation$objNum, labels = objNames)

# Sorting by participants and then by objects and splitting location and rating values into three separate variables
dataRatingObjectLocationAgg <- ddply(dataRatingObjectLocation, 
                                     c('subNum', 'objName', 'objNum'), 
                                     summarise, 
                                     location1 = location[1],  
                                     location2 = location[2],  
                                     location3 = location[3],
                                     rating1   = rating[1],
                                     rating2   = rating[2],
                                     rating3   = rating[3])

# Doing the same of the 3AFC data. 
dataAfcAgg <- ddply(dataAFC, 
                    c('subNum', 'objName', 'objNum'), 
                    summarise, 
                    foil1Location  = foil1Location, 
                    targetLocation = targetLocation, 
                    foil2Location  = foil2Location)

# Finding and assingin gthe respective ratings for targets and foils because this information 
# is not saved for the ratingTask data set. This is done by looking for the instances where the first, second or third
# location is equal to the target/foil 1/foil 2 location and assign the respective rating to that position.
# For target
objectLocationTarget  <- rep(NA, 20)
objectLocationTarget[which(dataRatingObjectLocationAgg$location1 == dataAfcAgg$targetLocation)] <- dataRatingObjectLocationAgg$rating1[which(dataRatingObjectLocationAgg$location1 == dataAfcAgg$targetLocation)]
objectLocationTarget[which(dataRatingObjectLocationAgg$location2 == dataAfcAgg$targetLocation)] <- dataRatingObjectLocationAgg$rating2[which(dataRatingObjectLocationAgg$location2 == dataAfcAgg$targetLocation)]
objectLocationTarget[which(dataRatingObjectLocationAgg$location3 == dataAfcAgg$targetLocation)] <- dataRatingObjectLocationAgg$rating3[which(dataRatingObjectLocationAgg$location3 == dataAfcAgg$targetLocation)]

# For foil 1
objectLocationFoil1  <- rep(NA, 20)
objectLocationFoil1[which(dataRatingObjectLocationAgg$location1 == dataAfcAgg$foil1Location)] <- dataRatingObjectLocationAgg$rating1[which(dataRatingObjectLocationAgg$location1 == dataAfcAgg$foil1Location)]
objectLocationFoil1[which(dataRatingObjectLocationAgg$location2 == dataAfcAgg$foil1Location)] <- dataRatingObjectLocationAgg$rating2[which(dataRatingObjectLocationAgg$location2 == dataAfcAgg$foil1Location)]
objectLocationFoil1[which(dataRatingObjectLocationAgg$location3 == dataAfcAgg$foil1Location)] <- dataRatingObjectLocationAgg$rating3[which(dataRatingObjectLocationAgg$location3 == dataAfcAgg$foil1Location)]

# For foil 2
objectLocationFoil2  <- rep(NA, 20)
objectLocationFoil2[which(dataRatingObjectLocationAgg$location1 == dataAfcAgg$foil2Location)] <- dataRatingObjectLocationAgg$rating1[which(dataRatingObjectLocationAgg$location1 == dataAfcAgg$foil2Location)]
objectLocationFoil2[which(dataRatingObjectLocationAgg$location2 == dataAfcAgg$foil2Location)] <- dataRatingObjectLocationAgg$rating2[which(dataRatingObjectLocationAgg$location2 == dataAfcAgg$foil2Location)]
objectLocationFoil2[which(dataRatingObjectLocationAgg$location3 == dataAfcAgg$foil2Location)] <- dataRatingObjectLocationAgg$rating3[which(dataRatingObjectLocationAgg$location3 == dataAfcAgg$foil2Location)]

########### Loading data from general ratings (post-encoding).
dataRatingGeneral         <- rbindlist(lapply(paste('data/ratingGeneral_', as.character(acceptedEntries),'.dat', sep = ''), fread))
names(dataRatingGeneral)  <- c('subNum', 
                               'date', 
                               'time', 
                               'trial',
                               'objNum', 
                               'location', 
                               'rating', 
                               'RT')
dataRatingGeneral$objName <- factor(dataRatingGeneral$objNum, labels = objNames)

######################################################################################
# Merging all datasets to one.
# The new dataset needs to be sorted by subNum and then by objNum
newCombData <- data.frame(subNum             = rep(demographics$subNum, each = 20),
                          date               = rep(demographics$date, each = 20),
                          gender             = rep(demographics$gender, each = 20),
                          age                = rep(demographics$age, each = 20),
                          startExp           = rep(demographics$startExp, each = 20),
                          endExp             = rep(demographics$endExp, each = 20),
                          objNum             = rep(1:20, length(acceptedEntries)),
                          objName            = rep(objNames, length(acceptedEntries)), 
                          recallTrial        = ddply(dataLocationRecall, c('subNum', 'objNum'), summarise, trial = trial)$trial,
                          startTimeRecall    = ddply(dataLocationRecall, c('subNum', 'objNum'), summarise, startTime = startTime)$startTime,
                          pickUpTime         = ddply(dataLocationRecall, c('subNum', 'objNum'), summarise, pickUpTime = pickUpTime)$pickUpTime,
                          dropDownTime       = ddply(dataLocationRecall, c('subNum', 'objNum'), summarise, dropDownTime = dropDownTime)$dropDownTime,
                          respTime           = ddply(dataLocationRecall, c('subNum', 'objNum'), summarise, respTime = respTime)$respTime,
                          answerTime         = ddply(dataLocationRecall, c('subNum', 'objNum'), summarise, answerTime = answerTime)$answerTime,
                          numAttempts        = ddply(dataLocationRecall, c('subNum', 'objNum'), summarise, numAttempts = numAttempts)$numAttempts,
                          recallNoMemory     = ddply(dataLocationRecall, c('subNum', 'objNum'), summarise, noMemory = noMemory)$noMemory,
                          xRetrieved         = ddply(dataLocationRecall, c('subNum', 'objNum'), summarise, xRetrieved = xRetrieved)$xRetrieved,
                          yRetrieved         = ddply(dataLocationRecall, c('subNum', 'objNum'), summarise, yRetrieved = yRetrieved)$yRetrieved,
                          zRetrieved         = ddply(dataLocationRecall, c('subNum', 'objNum'), summarise, zRetrieved = zRetrieved)$zRetrieved,
                          xPositionCorrect   = ddply(dataLocationRecall, c('subNum', 'objNum'), summarise, xPositionCorrect = xPositionCorrect)$xPositionCorrect,
                          yPositionCorrect   = ddply(dataLocationRecall, c('subNum', 'objNum'), summarise, yPositionCorrect = yPositionCorrect)$yPositionCorrect,
                          zPositionCorrect   = ddply(dataLocationRecall, c('subNum', 'objNum'), summarise, zPositionCorrect = zPositionCorrect)$zPositionCorrect,
                          euclideanDist      = ddply(dataLocationRecall, c('subNum', 'objNum'), summarise, euclideanDist = euclideanDist)$euclideanDist, 
                          targetLocation     = ddply(dataAFC, c('subNum', 'objNum'), summarise, targetLocation = targetLocation)$targetLocation,
                          targetRankPre      = ddply(dataAFC, c('subNum', 'objNum'), summarise, targetRankPre = targetRankPre)$targetRankPre,
                          foil1Location      = ddply(dataAFC, c('subNum', 'objNum'), summarise, foil1Location = foil1Location)$foil1Location,
                          foil1RankPre       = ddply(dataAFC, c('subNum', 'objNum'), summarise, foil1RankPre = foil1RankPre)$foil1RankPre,
                          foil2Location      = ddply(dataAFC, c('subNum', 'objNum'), summarise, foil2Location = foil2Location)$foil2Location,
                          foil2RankPre       = ddply(dataAFC, c('subNum', 'objNum'), summarise, foil2RankPre = foil2RankPre)$foil2RankPre,
                          AFCPreTime         = ddply(dataAFC, c('subNum', 'objNum'), summarise, AFCPreTime = AFCPreTime)$AFCPreTime,
                          rtAFC              = ddply(dataAFC, c('subNum', 'objNum'), summarise, rtAFC = rtAFC)$rtAFC,
                          resAFC             = ddply(dataAFC, c('subNum', 'objNum'), summarise, resAFC = resAFC)$resAFC,
                          accAFC             = ddply(dataAFC, c('subNum', 'objNum'), summarise, accAFC = accAFC)$accAFC,
                          conPreTime         = ddply(dataAFC, c('subNum', 'objNum'), summarise, conPreTime = conPreTime)$conPreTime,
                          rtCon              = ddply(dataAFC, c('subNum', 'objNum'), summarise, rtCon = rtCon)$rtCon,
                          resCon             = ddply(dataAFC, c('subNum', 'objNum'), summarise, resCon = resCon)$resCon,
                          left               = ddply(dataAFC, c('subNum', 'objNum'), summarise, left = left)$left,
                          middle             = ddply(dataAFC, c('subNum', 'objNum'), summarise, middle = middle)$middle,
                          right              = ddply(dataAFC, c('subNum', 'objNum'), summarise, right = right)$right,
                          afcTrial           = ddply(dataAFC, c('subNum', 'objNum'), summarise, trial = trial)$trial,
                          objLocTargetRating = objectLocationTarget,
                          objLocFoil1Rating  = objectLocationFoil1,
                          objLocFoil2Rating  = objectLocationFoil2,
                          generalRatingPost  = ddply(dataRatingGeneral, c('subNum', 'objNum'), summarise, rating = rating)$rating,
                          generalRatingNorm  = rep(normativeData$general, length(acceptedEntries)),
                          loc1               = rep(normativeData$loc1, length(acceptedEntries)),
                          loc2               = rep(normativeData$loc2, length(acceptedEntries)),
                          loc3               = rep(normativeData$loc3, length(acceptedEntries)),
                          loc4               = rep(normativeData$loc4, length(acceptedEntries)),
                          loc5               = rep(normativeData$loc5, length(acceptedEntries)),
                          loc6               = rep(normativeData$loc6, length(acceptedEntries)),
                          loc7               = rep(normativeData$loc7, length(acceptedEntries)),
                          loc8               = rep(normativeData$loc8, length(acceptedEntries)),
                          loc9               = rep(normativeData$loc9, length(acceptedEntries)),
                          loc10              = rep(normativeData$loc10, length(acceptedEntries)),
                          loc11              = rep(normativeData$loc11, length(acceptedEntries)),
                          loc12              = rep(normativeData$loc12, length(acceptedEntries)),
                          loc13              = rep(normativeData$loc13, length(acceptedEntries)),
                          loc14              = rep(normativeData$loc14, length(acceptedEntries)),
                          loc15              = rep(normativeData$loc15, length(acceptedEntries)),
                          loc16              = rep(normativeData$loc16, length(acceptedEntries)),
                          loc17              = rep(normativeData$loc17, length(acceptedEntries)),
                          loc18              = rep(normativeData$loc18, length(acceptedEntries)),
                          loc19              = rep(normativeData$loc19, length(acceptedEntries)),
                          loc20              = rep(normativeData$loc20, length(acceptedEntries)))
######################################################################################
# Further calculations on the dataset
# Calculating recall accuracy.
# This is calculated by finding the closest spawnPoint and then checking whether it was the correct one. 
newCombData$accRecall  <- -99
newCombData$closestLoc <- -99
newCombData$next2Loc   <- -99
newCombData$next3Loc   <- -99
newCombData$next4Loc   <- -99
newCombData$next5Loc   <- -99
newCombData$next6Loc   <- -99
newCombData$next7Loc   <- -99
newCombData$next8Loc   <- -99
newCombData$next9Loc   <- -99
newCombData$next10Loc  <- -99
newCombData$next11Loc  <- -99
newCombData$next12Loc  <- -99
newCombData$next13Loc  <- -99
newCombData$next14Loc  <- -99
newCombData$next15Loc  <- -99
newCombData$next16Loc  <- -99
newCombData$next17Loc  <- -99
newCombData$next18Loc  <- -99
newCombData$next19Loc  <- -99
newCombData$next20Loc  <- -99
for(i in 1:dim(dataLocationRecall)[1]){
  if(!is.na(newCombData$xRetrieved[i])){
    # If the values are not NA.
    # Calculating Euclidea distance between participants' placement and all twenty locations (i.e. spawn points).
    distances             <- euclideanDistance3D(newCombData$xRetrieved[i],
                                                 newCombData$yRetrieved[i],
                                                 newCombData$zRetrieved[i],
                                                 spawnPoints2$x,
                                                 spawnPoints2$y,
                                                 spawnPoints2$z)
    
    # Saving the closest spawn point as well as the second, third and so on closet spawn point.
    newCombData$closestLoc[i] <- as.numeric(spawnPoints2[order(distances)[1], 1])
    newCombData$next2Loc[i]   <- as.numeric(spawnPoints2[order(distances)[2], 1])
    newCombData$next3Loc[i]   <- as.numeric(spawnPoints2[order(distances)[3], 1])
    newCombData$next4Loc[i]   <- as.numeric(spawnPoints2[order(distances)[4], 1])
    newCombData$next5Loc[i]   <- as.numeric(spawnPoints2[order(distances)[5], 1])
    newCombData$next6Loc[i]   <- as.numeric(spawnPoints2[order(distances)[6], 1])
    newCombData$next7Loc[i]   <- as.numeric(spawnPoints2[order(distances)[7], 1])
    newCombData$next8Loc[i]   <- as.numeric(spawnPoints2[order(distances)[8], 1])
    newCombData$next9Loc[i]   <- as.numeric(spawnPoints2[order(distances)[9], 1])
    newCombData$next10Loc[i]  <- as.numeric(spawnPoints2[order(distances)[10], 1])
    newCombData$next11Loc[i]  <- as.numeric(spawnPoints2[order(distances)[11], 1])
    newCombData$next12Loc[i]  <- as.numeric(spawnPoints2[order(distances)[12], 1])
    newCombData$next13Loc[i]  <- as.numeric(spawnPoints2[order(distances)[13], 1])
    newCombData$next14Loc[i]  <- as.numeric(spawnPoints2[order(distances)[14], 1])
    newCombData$next15Loc[i]  <- as.numeric(spawnPoints2[order(distances)[15], 1])
    newCombData$next16Loc[i]  <- as.numeric(spawnPoints2[order(distances)[16], 1])
    newCombData$next17Loc[i]  <- as.numeric(spawnPoints2[order(distances)[17], 1])
    newCombData$next18Loc[i]  <- as.numeric(spawnPoints2[order(distances)[18], 1])
    newCombData$next19Loc[i]  <- as.numeric(spawnPoints2[order(distances)[19], 1])
    newCombData$next20Loc[i]  <- as.numeric(spawnPoints2[order(distances)[20], 1])
    
    if(newCombData$closestLoc[i] == newCombData$targetLocation[i]){
      # If closest lcoation is target location.
      newCombData$accRecall[i] <- 1
    } else {
      # If closet lcoation is not target location.
      newCombData$accRecall[i] <- 0
    } 
  } else {
    # If the values are NA
    # Saving the closest spawn point as well as the second, third and so on closet spawn point
    newCombData$closestLoc[i] <- NA
    newCombData$accRecall[i]  <- NA
    newCombData$next2Loc[i]   <- NA
    newCombData$next3Loc[i]   <- NA
    newCombData$next4Loc[i]   <- NA
    newCombData$next5Loc[i]   <- NA
    newCombData$next6Loc[i]   <- NA
    newCombData$next7Loc[i]   <- NA
    newCombData$next8Loc[i]   <- NA
    newCombData$next9Loc[i]   <- NA
    newCombData$next10Loc[i]  <- NA
    newCombData$next11Loc[i]  <- NA
    newCombData$next12Loc[i]  <- NA
    newCombData$next13Loc[i]  <- NA
    newCombData$next14Loc[i]  <- NA
    newCombData$next15Loc[i]  <- NA
    newCombData$next16Loc[i]  <- NA
    newCombData$next17Loc[i]  <- NA
    newCombData$next18Loc[i]  <- NA
    newCombData$next19Loc[i]  <- NA
    newCombData$next20Loc[i]  <- NA
  }
}
######################################################################################
# Loading existing dataset and creating a back up file
load("data/mergedData/exp1Data.RData")
save(combData, file = paste("data/mergedData/exp1Data_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".RData"))
write.table(combData, 
            file = paste("data/mergedData/exp1Data_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".txt"), 
            col.names = TRUE, 
            row.names = FALSE, 
            quote = FALSE,
            sep = "\t")
######################################################################################
# Combining new with old data and saving all files
combData <- rbind(combData, newCombData)
save(combData, file = "data/mergedData/exp1Data.RData")
write.table(combData, 
            file = 'exp1Data.txt', 
            col.names = TRUE, 
            row.names = FALSE, 
            quote = FALSE,
            sep = "\t")
######################################################################################
# Appendix:
# Legend of variables names:
# subNum                Subject/participant number (e.g. 1, 2, 3...).
# date                  Date of session in YYYYMMDD format.
# gender                Gender of participna (0 = female, 1 = male, 3 = non-binary).
# age                   Age of participnat in years.
# startExp              Time the experiment started in HHMM format. 
# endExp                Time the experiment ended in HHMM format. 
# objNum                Object number (e.g. 1, 2, 3...).
# objName               Object name (e.g. helmet).
# recallTrial           Trial number during location recall task.
# startTimeRecall       Time (in seconds) the trial started  in relation to starting the scene in unity.
# pickUpTime            Time (in seconds) the object was picked up in relation to starting the scene in unity.
# dropDownTime          Time (in seconds) the object was dropped down in relation to starting the scene in unity.
# respTime              Time (in seconds) the participant needed to respond (i.e. picking the object up) from start of trial. 
# answerTime            Time (in seconds) the participant needed to give answer (i.e. droping the object) from the time the object of picked up. 
# numAttempts           Number of attempts. If an object was lost, the trial could be started again. 
# recallNoMemory        No memory trial is a trial on which the participant told the experimenter that he/she did not see the object (0 = did not report not seeing object, 1 = repoted not seeing the object).
# xRetrieved            X position (in virtual metres), where the participant placed the object.
# yRetrieved            Y position (in virtual metres), where the participant placed the object.
# zRetrieved            Z position (in virtual metres), where the participant placed the object.
# xPositionCorrect      X position (in virtual metres), where the object actually was.
# yPositionCorrect      Y position (in virtual metres), where the object actually was.
# zPositionCorrect      Z position (in virtual metres), where the object actually was.
# euclideanDist         Euclidean distance (in virtual metres) between point, where the object was placed and where the object actually was. 
# targetLocation        Number of the location, where the object actually was (i.e. tartget location).
# targetRankPre         Average rank from normative dataset for the object/location ratings for the target (0 to 400).
# foil1Location         Number of the location of foil 1 in 3AFC task.
# foil1RankPre          Average rank from normative dataset for the object/location ratings for foil 1 (0 to 400).
# foil2Location         Number of the location of foil 2 in 3AFC task.
# foil2RankPre          Average rank from normative dataset for the object/location ratings for foil 2 (0 to 400).
# AFCPreTime            Presentation time (in milliseconds) of the three images in the 3AFC task.
# rtAFC                 Reaction time (in milliseconds) to make 3AFC decision. 
# resAFC                Response given for 3AFC decision (1, 2 or 3).
# accAFC                Accuracy of 3AFC response (0 = incorrect, 1 = correct).
# conPreTime            Presentation time (in milliseconds) of confidence display in the 3AFC task.
# rtCon                 Reaction time (in milliseconds) to make confidence decision.
# resCon                Response given for confidence decision (1 = Did not see object, 2 = Guess the object was there or 3 = Know the object was there).
# left                  Number of the location (i.e. image) that was displayed on the left side of the screen.
# middle                Number of the location (i.e. image) that was displayed in the middle of the screen.
# right                 Number of the location (i.e. image) that was displayed on the right side of the screen.
# afcTrial              Trial number during 3AFC task.
# objLocTargetRating    Object/location expectancy rating (unexpected/-100 to expected/100) by the participant for object in target location.
# objLocFoil1Rating     Object/location expectancy rating (unexpected/-100 to expected/100) by the participant for object in target foil 1 location.
# objLocFoil2Rating     Object/location expectancy rating (unexpected/-100 to expected/100) by the participant for object in target foil 2 location.
# generalRatingPost     General expectancy rating (unexpected/-100 to expected/100) by the participant for object.
# generalRatingNorm     Average general expectancy rating (unexpected/-100 to expected/100) from normative dataset for object.
# loc1 to loc20         Average object/location rating (unexpected/-100 to expected/100) for object in location 1 to 20 from normative dataset.
# accRecall             Accuracy of recall (0 = incorrect, 1 = correct). This is calculated by finding the closest spawnPoint and then checking whether it was the correct one. 
# closestLoc            Location with the shortest Euclidean distance for the point, where the object was placed. 
# next2Loc to next20Loc Second to twentieth closest location (based on Euclidean distance). 