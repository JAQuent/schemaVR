######################################################################################
# Preapare dataset for schemaVR3 
# Version 1.0
# Date:28/11/2018
# Author: Joern Alexander Quent
######################################################################################
# Setting and libraries
setwd("U:/Projects/schemaVR/schemaVR3/analysis")
library(data.table)
library(plyr)

# Subject and set numbers
subNum   <- c(1:25)
N        <- length(subNum)
setNum   <- c(111, 246, 388, 498, 848)
# File locations and names for data files
fileNam1 <- paste('U:/Projects/schemaVR/schemaVR3/data/schemaVR3_recall_', subNum, '.txt', sep = "")
fileNam2 <- paste('U:/Projects/schemaVR/schemaVR3/data/correctionValues_', subNum, '.txt', sep = "")
fileNam3 <- paste('U:/Projects/schemaVR/schemaVR3/data/schemaVR3_encodingInputFile_VR_', setNum, '.txt', sep = "")
fileNam4 <- paste('U:/Projects/schemaVR/schemaVR3/data/schemaVR3_3AFC_inputFile_', setNum, '.txt', sep = "")
fileNam5 <- paste('U:/Projects/schemaVR/schemaVR3/data/ratingObjLoc_', subNum, '.dat', sep = "")
fileNam6 <- paste('U:/Projects/schemaVR/schemaVR3/data/ratingGen_', subNum, '.dat', sep = "")
fileNam7 <- paste('U:/Projects/schemaVR/schemaVR3/data/retrievalTask_', subNum, '.dat', sep = "")
fileNam8 <- paste('U:/Projects/schemaVR/schemaVR3/data/demographic_', subNum, '.txt', sep = "")

# Object names
objNam <- c('microwave',
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

######################################################################################
########### Demographics.
demographics        <- as.data.frame(rbindlist(lapply(fileNam8, fread)))
names(demographics) <- c('subNum',
                         'setNum',
                         'gender',
                         'age',
                         'date',
                         'startExp',
                         'endExp')

######################################################################################
########### Recall data
# Due to a mistake in the script the dropDownTime and hence the answerTime are not saved.

# Loading recall data
dataRecall        <- as.data.frame(rbindlist(lapply(fileNam1, fread)))
names(dataRecall) <- c('subNum',
                       'trial',
                       'set',
                       'startTime',
                       'pickUpTime',
                       'dropDownTime',
                       'objNam',
                       'objNum',
                       'respTime',
                       'answerTime',
                       'numAttempts',
                       'memory',
                       'xPlaced',
                       'yPlaced',
                       'zPlaced')

# Correcting a mislabel mistake from the unity experiment
dataRecall$objNum[dataRecall$objNam == 'glassContainer'] <- 10
dataRecall$objNum[dataRecall$objNam == 'mug']            <- 11
dataRecall$objNum[dataRecall$objNam == 'dishes']         <- 12

# Loading correctionValues
# These  are generated in unity3d to align the virtual room with the 
# participant's current position in the room set-up. This is important to calculate recall performance
correVal        <- as.data.frame(rbindlist(lapply(fileNam2, fread)))
names(correVal) <- c('x', 'z')

# Loading encoding locations containing the non-corrected x, y and z coordinates where the objects appear in VR.
encodLoc        <- as.data.frame(rbindlist(lapply(fileNam3, fread)))
names(encodLoc) <- c('objNam', 'xRot', 'yRot', 'zRot', 'xPos', 'yPos', 'zPos')
encodLoc$setNum <- rep(setNum, each = 20)
encodLoc$objNum <- rep(1:20, length(setNum))

# Replacing -99 in memory with NA values.
dataRecall$memory[dataRecall$memory == -999] <- NA_integer_

# Sort by subNum and then by objNum
dataRecall     <- dataRecall[order(dataRecall$subNum, dataRecall$objNum),]

# Using a for loop to add encoding locations and the number of it for each participant
AFC_input <- as.data.frame(rbindlist(lapply(fileNam4, fread)))
names(AFC_input) <- c('objNum', 
                      'foil1Rank', 
                      'encodingRank', 
                      'foil2Rank', 
                      'foil1Location', 
                      'encodingLocation', 
                      'foil2Location')
AFC_input$setNum <- rep(setNum, each = 20)

dataRecall$xPosCorr <- NA_real_
dataRecall$yPosCorr <- NA_real_
dataRecall$zPosCorr <- NA_real_
dataRecall$encodLoc <- NA_real_
for(i in subNum){
  dataRecall[which(dataRecall$subNum == i), 'xPosCorr'] <- encodLoc[encodLoc$setNum == dataRecall[dataRecall$subNum == i, 'set'][1], 'xPos']
  dataRecall[which(dataRecall$subNum == i), 'yPosCorr'] <- encodLoc[encodLoc$setNum == dataRecall[dataRecall$subNum == i, 'set'][1], 'yPos']
  dataRecall[which(dataRecall$subNum == i), 'zPosCorr'] <- encodLoc[encodLoc$setNum == dataRecall[dataRecall$subNum == i, 'set'][1], 'zPos']
  dataRecall[which(dataRecall$subNum == i), 'encodLoc'] <- AFC_input[AFC_input$setNum == dataRecall[dataRecall$subNum == i, 'set'][1], 'encodingLocation']
}

# Correct values
dataRecall$xPosCorr <- dataRecall$xPosCorr - rep(correVal$x, each =  20)
dataRecall$zPosCorr <- dataRecall$zPosCorr - rep(correVal$z, each =  20)


########### Calculating Euclidean distance
euclideanDistance3D <-function(x1, y1, z1, x2, y2, z2){
  return(sqrt((x1 - x2)^2 + (y1 - y2)^2 + (z1 - z2)^2))
} # This function is used to calculate the Euclidean distance in 3D space

dataRecall$euclideanDist <- euclideanDistance3D(dataRecall$xPlaced, 
                                                dataRecall$yPlaced, 
                                                dataRecall$zPlaced, 
                                                dataRecall$xPosCorr, 
                                                dataRecall$yPosCorr, 
                                                dataRecall$zPosCorr)

########### Calculating recall accuracy.
# This is calculated by finding the closest spawnPoint and then checking whether it was the correct one. 
# Due to this I need to find the number of the location used, which I did above. 
dataRecall$accRecall  <- NA
dataRecall$closestLoc <- NA
dataRecall$next2Loc   <- NA
dataRecall$next3Loc   <- NA
dataRecall$next4Loc   <- NA
dataRecall$next5Loc   <- NA
dataRecall$next6Loc   <- NA
dataRecall$next7Loc   <- NA
dataRecall$next8Loc   <- NA
dataRecall$next9Loc   <- NA
dataRecall$next10Loc  <- NA
dataRecall$next11Loc  <- NA
dataRecall$next12Loc  <- NA
dataRecall$next13Loc  <- NA
dataRecall$next14Loc  <- NA
dataRecall$next15Loc  <- NA
dataRecall$next16Loc  <- NA
dataRecall$next17Loc  <- NA
dataRecall$next18Loc  <- NA
dataRecall$next19Loc  <- NA
dataRecall$next20Loc  <- NA
for(i in 1:dim(dataRecall)[1]){
  if(!is.na(dataRecall$xPlaced[i])){
    # If the values are not NA.
    # Calculating Euclidea distance between participants' placement and all twenty locations (i.e. spawn points).
    tempData    <- subset(dataRecall, subNum == dataRecall$subNum[i])
    spawnPoints <-  data.frame(point = tempData$encodLoc, 
                               x = tempData$xPosCorr,
                               y = tempData$yPosCorr,
                               z = tempData$zPosCorr)
    distances             <- euclideanDistance3D(dataRecall$xPlaced[i],
                                                 dataRecall$yPlaced[i],
                                                 dataRecall$zPlaced[i],
                                                 spawnPoints$x,
                                                 spawnPoints$y,
                                                 spawnPoints$z)
    
    # Saving the closest spawn point as well as the second, third and so on closet spawn point.
    dataRecall$closestLoc[i] <- as.numeric(spawnPoints[order(distances)[1], 1])
    dataRecall$next2Loc[i]   <- as.numeric(spawnPoints[order(distances)[2], 1])
    dataRecall$next3Loc[i]   <- as.numeric(spawnPoints[order(distances)[3], 1])
    dataRecall$next4Loc[i]   <- as.numeric(spawnPoints[order(distances)[4], 1])
    dataRecall$next5Loc[i]   <- as.numeric(spawnPoints[order(distances)[5], 1])
    dataRecall$next6Loc[i]   <- as.numeric(spawnPoints[order(distances)[6], 1])
    dataRecall$next7Loc[i]   <- as.numeric(spawnPoints[order(distances)[7], 1])
    dataRecall$next8Loc[i]   <- as.numeric(spawnPoints[order(distances)[8], 1])
    dataRecall$next9Loc[i]   <- as.numeric(spawnPoints[order(distances)[9], 1])
    dataRecall$next10Loc[i]  <- as.numeric(spawnPoints[order(distances)[10], 1])
    dataRecall$next11Loc[i]  <- as.numeric(spawnPoints[order(distances)[11], 1])
    dataRecall$next12Loc[i]  <- as.numeric(spawnPoints[order(distances)[12], 1])
    dataRecall$next13Loc[i]  <- as.numeric(spawnPoints[order(distances)[13], 1])
    dataRecall$next14Loc[i]  <- as.numeric(spawnPoints[order(distances)[14], 1])
    dataRecall$next15Loc[i]  <- as.numeric(spawnPoints[order(distances)[15], 1])
    dataRecall$next16Loc[i]  <- as.numeric(spawnPoints[order(distances)[16], 1])
    dataRecall$next17Loc[i]  <- as.numeric(spawnPoints[order(distances)[17], 1])
    dataRecall$next18Loc[i]  <- as.numeric(spawnPoints[order(distances)[18], 1])
    dataRecall$next19Loc[i]  <- as.numeric(spawnPoints[order(distances)[19], 1])
    dataRecall$next20Loc[i]  <- as.numeric(spawnPoints[order(distances)[20], 1])
    
    if(dataRecall$closestLoc[i] == dataRecall$encodLoc[i]){
      # If closest lcoation is target location.
      dataRecall$accRecall[i] <- 1
    } else {
      # If closet lcoation is not target location.
      dataRecall$accRecall[i] <- 0
    } 
  } else {
    # If the values are NA
    # Saving the closest spawn point as well as the second, third and so on closet spawn point
    dataRecall$closestLoc[i] <- NA
    dataRecall$accRecall[i]  <- NA
    dataRecall$next2Loc[i]   <- NA
    dataRecall$next3Loc[i]   <- NA
    dataRecall$next4Loc[i]   <- NA
    dataRecall$next5Loc[i]   <- NA
    dataRecall$next6Loc[i]   <- NA
    dataRecall$next7Loc[i]   <- NA
    dataRecall$next8Loc[i]   <- NA
    dataRecall$next9Loc[i]   <- NA
    dataRecall$next10Loc[i]  <- NA
    dataRecall$next11Loc[i]  <- NA
    dataRecall$next12Loc[i]  <- NA
    dataRecall$next13Loc[i]  <- NA
    dataRecall$next14Loc[i]  <- NA
    dataRecall$next15Loc[i]  <- NA
    dataRecall$next16Loc[i]  <- NA
    dataRecall$next17Loc[i]  <- NA
    dataRecall$next18Loc[i]  <- NA
    dataRecall$next19Loc[i]  <- NA
    dataRecall$next20Loc[i]  <- NA
  }
}

########### Loading data from 3AFC task
dataAFC <- as.data.frame(rbindlist(lapply(fileNam7, fread)))
names(dataAFC) <- c('subNum',
                    'setNum',
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
dataAFC$objNam     <- factor(dataAFC$objNum, labels = objNam)
dataAFC$left       <- factor(dataAFC$left, labels = c('target', 'foil1', 'foil2'))
dataAFC$middle     <- factor(dataAFC$middle, labels = c('target', 'foil1', 'foil2'))
dataAFC$right      <- factor(dataAFC$right, labels = c('target', 'foil1', 'foil2'))
dataAFC$resCon     <- as.factor(dataAFC$resCon)

########### Loading data from object/location ratings (post-encoding).
dataRatingObjectLocation         <- as.data.frame(rbindlist(lapply(fileNam5, fread)))
names(dataRatingObjectLocation)  <- c('subNum', 
                                      'date', 
                                      'time', 
                                      'trial',
                                      'objNum', 
                                      'location', 
                                      'rating', 
                                      'RT')
dataRatingObjectLocation$objNam <- factor(dataRatingObjectLocation$objNum, labels = objNam)

# Sorting by participants and then by objects and splitting location and rating values into three separate variables
dataRatingObjectLocationAgg <- ddply(dataRatingObjectLocation, 
                                     c('subNum', 'objNam', 'objNum'), 
                                     summarise, 
                                     location1 = location[1],  
                                     location2 = location[2],  
                                     location3 = location[3],
                                     rating1   = rating[1],
                                     rating2   = rating[2],
                                     rating3   = rating[3])

# Doing the same of the 3AFC data. 
dataAfcAgg <- ddply(dataAFC, 
                    c('subNum', 'objNam', 'objNum'), 
                    summarise, 
                    foil1Location  = foil1Location, 
                    targetLocation = targetLocation, 
                    foil2Location  = foil2Location)

# Finding and assingin the respective ratings for targets and foils because this information 
# is not saved for the ratingTask data set. This is done by looking for the instances where the first, second or third
# location is equal to the target/foil 1/foil 2 location and assign the respective rating to that position.
# For target
objectLocationTarget  <- rep(NA, dim(dataRatingObjectLocationAgg)[1])
objectLocationTarget[which(dataRatingObjectLocationAgg$location1 == dataAfcAgg$targetLocation)] <- dataRatingObjectLocationAgg$rating1[which(dataRatingObjectLocationAgg$location1 == dataAfcAgg$targetLocation)]
objectLocationTarget[which(dataRatingObjectLocationAgg$location2 == dataAfcAgg$targetLocation)] <- dataRatingObjectLocationAgg$rating2[which(dataRatingObjectLocationAgg$location2 == dataAfcAgg$targetLocation)]
objectLocationTarget[which(dataRatingObjectLocationAgg$location3 == dataAfcAgg$targetLocation)] <- dataRatingObjectLocationAgg$rating3[which(dataRatingObjectLocationAgg$location3 == dataAfcAgg$targetLocation)]

# For foil 1
objectLocationFoil1  <- rep(NA, dim(dataRatingObjectLocationAgg)[1])
objectLocationFoil1[which(dataRatingObjectLocationAgg$location1 == dataAfcAgg$foil1Location)] <- dataRatingObjectLocationAgg$rating1[which(dataRatingObjectLocationAgg$location1 == dataAfcAgg$foil1Location)]
objectLocationFoil1[which(dataRatingObjectLocationAgg$location2 == dataAfcAgg$foil1Location)] <- dataRatingObjectLocationAgg$rating2[which(dataRatingObjectLocationAgg$location2 == dataAfcAgg$foil1Location)]
objectLocationFoil1[which(dataRatingObjectLocationAgg$location3 == dataAfcAgg$foil1Location)] <- dataRatingObjectLocationAgg$rating3[which(dataRatingObjectLocationAgg$location3 == dataAfcAgg$foil1Location)]

# For foil 2
objectLocationFoil2  <- rep(NA, dim(dataRatingObjectLocationAgg)[1])
objectLocationFoil2[which(dataRatingObjectLocationAgg$location1 == dataAfcAgg$foil2Location)] <- dataRatingObjectLocationAgg$rating1[which(dataRatingObjectLocationAgg$location1 == dataAfcAgg$foil2Location)]
objectLocationFoil2[which(dataRatingObjectLocationAgg$location2 == dataAfcAgg$foil2Location)] <- dataRatingObjectLocationAgg$rating2[which(dataRatingObjectLocationAgg$location2 == dataAfcAgg$foil2Location)]
objectLocationFoil2[which(dataRatingObjectLocationAgg$location3 == dataAfcAgg$foil2Location)] <- dataRatingObjectLocationAgg$rating3[which(dataRatingObjectLocationAgg$location3 == dataAfcAgg$foil2Location)]

########### Loading data from general ratings (post-encoding).
dataRatingGeneral         <- as.data.frame(rbindlist(lapply(fileNam6, fread)))
names(dataRatingGeneral)  <- c('subNum', 
                               'date', 
                               'time', 
                               'trial',
                               'objNum', 
                               'location', 
                               'rating', 
                               'RT')
dataRatingGeneral$objName <- factor(dataRatingGeneral$objNum, labels = objNam)

######################################################################################
########### Combine to one data frame
# Sort AFC data
dataAFC <- dataAFC[order(dataAFC$subNum, dataAFC$objNum),]

# Creating general expectancy factor
kitchen                                <- rep('kitchen', dim(dataRecall)[1])
kitchen[which(dataRecall$objNum > 12)] <- 'non-kitchen'


# Create data frame
dataSchemaVR3 <- data.frame(subNum                  = rep(subNum, each = 20),
                            objNum                  = rep(1:20, N),
                            objNam                  = rep(objNam, N), 
                            setNum                  = dataRecall$set,
                            recallTrial             = dataRecall$trial,
                            startTimeRecall         = dataRecall$startTime,
                            pickUpTime              = dataRecall$pickUpTime,
                            dropDownTime            = dataRecall$dropDownTime,
                            respTime                = dataRecall$respTime,
                            answerTime              = dataRecall$answerTime,
                            numAttempts             = dataRecall$numAttempts,
                            recallMemory            = dataRecall$memory,
                            xRetrieved              = dataRecall$xPlaced,
                            yRetrieved              = dataRecall$yPlaced,
                            zRetrieved              = dataRecall$zPlaced,
                            xPositionCorrect        = dataRecall$xPosCorr,
                            yPositionCorrect        = dataRecall$yPosCorr,
                            zPositionCorrect        = dataRecall$zPosCorr,
                            euclideanDist           = dataRecall$euclideanDist, 
                            accRecall               = dataRecall$accRecall, 
                            targetLocation          = dataAFC$targetLocation,
                            targetRankPre           = dataAFC$targetRankPre,
                            foil1Location           = dataAFC$foil1Location,
                            foil1RankPre            = dataAFC$foil1RankPre,
                            foil2Location           = dataAFC$foil2Location,
                            foil2RankPre            = dataAFC$foil2RankPre,
                            AFCPreTime              = dataAFC$AFCPreTime,
                            rtAFC                   = dataAFC$rtAFC,
                            resAFC                  = dataAFC$resAFC,
                            accAFC                  = dataAFC$accAFC,
                            conPreTime              = dataAFC$conPreTime,
                            rtCon                   = dataAFC$rtCon,
                            resCon                  = dataAFC$resCon,
                            left                    = dataAFC$left,
                            middle                  = dataAFC$middle,
                            right                   = dataAFC$right,
                            afcTrial                = dataAFC$trial,
                            objLocTargetRating      = objectLocationTarget,
                            objLocFoil1Rating       = objectLocationFoil1,
                            objLocFoil2Rating       = objectLocationFoil2,
                            generalRatingPost       = ddply(dataRatingGeneral, c('subNum', 'objNum'), summarise, rating = rating)$rating,
                            kitchen                 = as.factor(kitchen),
                            closestLoc              = dataRecall$closestLoc)


save(dataSchemaVR3, file  = "U:/Projects/schemaVR/schemaVR3/data/dataSchemaVR3.RData")

# Scaling data for analysis
dataSchemaVR3_Scaled                    <- dataSchemaVR3
dataSchemaVR3_Scaled$objLocTargetRating <- scale(dataSchemaVR3_Scaled$objLocTargetRating)
dataSchemaVR3_Scaled$targetRankPre      <- scale(dataSchemaVR3_Scaled$targetRankPre)

dataSchemaVR3_Scaled$generalRatingPost  <- scale(dataSchemaVR3_Scaled$generalRatingPost)


# Subsetting data
dataSchemaVR3_AFC    <- subset(dataSchemaVR3_Scaled, dataSchemaVR3_Scaled$resCon != 0)
dataSchemaVR3_recall <- subset(dataSchemaVR3_Scaled, dataSchemaVR3_Scaled$recallMemory != 0 | !is.na(dataSchemaVR3_Scaled$recallMemory))


library(lmerTest)
model1 <- glmer(accAFC ~  objLocTargetRating +
                             I(objLocTargetRating*objLocTargetRating) + 
                  generalRatingPost +
                             (1 | subNum) +
                             (1 | objNum),
                           data = dataSchemaVR3_AFC,
                           family = binomial,
                           control = glmerControl(optimizer = "bobyqa"),
                           nAGQ = 1)
summary(model1)

model2 <- glmer(accRecall ~  objLocTargetRating +
                            I(objLocTargetRating*objLocTargetRating) +
                  generalRatingPost +
                            (1 | subNum) +
                            (1 | objNum),
                          data = dataSchemaVR3_recall,
                          family = binomial,
                          control = glmerControl(optimizer = "bobyqa"),
                          nAGQ = 1)
summary(model2)

model3 <- lmer(euclideanDist ~  objLocTargetRating +
                            I(objLocTargetRating*objLocTargetRating) +
                 generalRatingPost +
                            (1 | subNum) +
                            (1 | objNum),
                          data = dataSchemaVR3_recall)
summary(model3)

library(ggplot2)
ggplot(dataSchemaVR3_recall, aes(x = objLocTargetRating, y = euclideanDist)) + geom_point() + geom_smooth()
ggplot(dataSchemaVR3_recall, aes(x = objLocTargetRating, y = accRecall)) + geom_point() + geom_smooth()

# Aggregating
schemaVR3_table4 <- ddply(dataSchemaVR3, 
                          c('objNum', 'objNam', 'targetLocation'), 
                          summarise,
                          objLocTargetRating = mean(objLocTargetRating, na.rm = TRUE),
                          RecallNoMemory     = table(recallMemory)[1],
                          RecallRecollection = table(recallMemory)[2],
                          RecallFamiliarity  = table(recallMemory)[3],
                          RecallGuess        = table(recallMemory)[4],
                          RecallN            = sum(table(recallMemory)),
                          RecallRec_per      = (RecallRecollection/RecallN)*100,
                          RecallFam_per      = (RecallFamiliarity/RecallN)*100,
                          AFCNoMemory        = table(resCon)[1], 
                          AFCRecollection    = table(resCon)[2],
                          AFCFamiliarity     = table(resCon)[3],
                          AFCGuess           = table(resCon)[4],
                          AFCN               = sum(table(resCon)),
                          AFCRec_per         = (AFCRecollection/AFCN)*100,
                          AFCFam_per         = (AFCFamiliarity/AFCN)*100)

# Median split
objLoc_expectedness <- rep('expected',dim(schemaVR3_table4)[1])
objLoc_expectedness[which(schemaVR3_table4$objLocTargetRating <= median(schemaVR3_table4$objLocTargetRating))] <- 'unexpected'

schemaVR3_table4$objLoc_expectedness <- objLoc_expectedness


# Recollectioon
schemaVR2_cor6   <- cor.test( ~ RecallRec_per + objLocTargetRating, schemaVR3_table4)
schemaVR2_cor7   <- cor.test( ~ AFCRec_per + objLocTargetRating, schemaVR3_table4)
plot(schemaVR3_table4$objLocTargetRating, schemaVR3_table4$AFCRec_per)

# Familiarity
schemaVR2_cor8   <- cor.test( ~ RecallFam_per + objLocTargetRating, schemaVR3_table4)
schemaVR2_cor9   <- cor.test( ~ AFCFam_per + objLocTargetRating, schemaVR3_table4)