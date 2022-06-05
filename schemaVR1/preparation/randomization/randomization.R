setwd("U:/Projects/Exp1/variousStuff/randomization")
#setwd("~/ownCloud/randomization")

data <- read.table('objectList.txt', header = TRUE)
n                          <- 20
numRelevantObjects         <- 12
numIrrelevantObjects       <- 8
numRelevantObjectsTested   <- 8
numFoils                   <- 8
numIrrelevantObjectsTested <- 4
relevantObjects            <- subset(data, relevancy == 'relevant')
irrelevantObjects          <- subset(data, relevancy == 'irrelevant')
expectednessEncoding       <- rep(c('expected', 'expected', 'unexpected', 'unexpected'), each = 2)
expectednessRetrieval      <- c('expected', 'expected', 'unexpected', 'unexpected')
possibleLocations          <- paste('spawnPoint', 1:20, sep = '')
allNames                   <- c('microwave', 'kitchenRoll', 'saucepan', 'toaster', 'fruitBowl', 'teaPot', 'knife', 'mixer', 'bread', 'dishes', 'glassContainer', 'mug', 'towel', 'toy', 'bookPile', 'umbrella', 'hat', 'helmet', 'calendar','fan')


stimLists                  <- c()
stimListRetrievals         <- c()

for(subject in 3:n){
  relevantObjectsTested                    <- relevantObjects[sample(1:numRelevantObjects, numRelevantObjectsTested),]
  irrelevantObjectsTested                  <- irrelevantObjects[sample(1:numIrrelevantObjects, numIrrelevantObjectsTested),]
  relevantObjectsTested$encodingCondition  <- sample(expectednessEncoding)
  relevantObjectsTested$retrievalCondition <- ''
  
  relevantObjectsTested$retrievalCondition[which(relevantObjectsTested$encodingCondition == 'expected')]   <- sample(expectednessRetrieval)
  relevantObjectsTested$retrievalCondition[which(relevantObjectsTested$encodingCondition == 'unexpected')] <- sample(expectednessRetrieval)
  relevantObjectsTested$encodingLocation <- ''
  relevantObjectsTested$encodingRotation <- ''
  relevantObjectsTested$expected1        <- as.character(relevantObjectsTested$expected1)
  relevantObjectsTested$expected2        <- as.character(relevantObjectsTested$expected2)
  relevantObjectsTested$expected3        <- as.character(relevantObjectsTested$expected3)
  relevantObjectsTested$unexpected1      <- as.character(relevantObjectsTested$unexpected1)
  relevantObjectsTested$unexpected2      <- as.character(relevantObjectsTested$unexpected2)
  relevantObjectsTested$unexpected3      <- as.character(relevantObjectsTested$unexpected3)
  relevantObjectsTested$expected1.rot    <- as.numeric(as.character(relevantObjectsTested$expected1.rot))
  relevantObjectsTested$expected2.rot    <- as.numeric(as.character(relevantObjectsTested$expected2.rot))
  relevantObjectsTested$expected3.rot    <- as.numeric(as.character(relevantObjectsTested$expected3.rot))
  relevantObjectsTested$unexpected1.rot  <- as.numeric(as.character(relevantObjectsTested$unexpected1.rot))
  relevantObjectsTested$unexpected2.rot  <- as.numeric(as.character(relevantObjectsTested$unexpected2.rot))
  relevantObjectsTested$unexpected3.rot  <- as.numeric(as.character(relevantObjectsTested$unexpected3.rot))
  while(max(table(relevantObjectsTested$encodingLocation)) > 1){
    count <- 1
    while(max(table(relevantObjectsTested$encodingLocation)) > 1 & count < 100){
      for(i in 1:numRelevantObjectsTested){
        print(max(table(relevantObjectsTested$encodingLocation)))
        randomLocation <- sample(1:3, 1)
        if(randomLocation == 1){
          if(relevantObjectsTested$encodingCondition[i] == 'expected'){
            relevantObjectsTested$encodingLocation[i] <- relevantObjectsTested$expected1[i]
            relevantObjectsTested$encodingRotation[i] <- relevantObjectsTested$expected1.rot[i]
          } else {
            relevantObjectsTested$encodingLocation[i] <- relevantObjectsTested$unexpected1[i]
            relevantObjectsTested$encodingRotation[i] <- relevantObjectsTested$unexpected1.rot[i]
          }
        } else if(randomLocation == 2){
          if(relevantObjectsTested$encodingCondition[i] == 'expected'){
            relevantObjectsTested$encodingLocation[i] <- relevantObjectsTested$expected2[i]
            relevantObjectsTested$encodingRotation[i] <- relevantObjectsTested$expected2.rot[i]
          } else {
            relevantObjectsTested$encodingLocation[i] <- relevantObjectsTested$unexpected2[i]
            relevantObjectsTested$encodingRotation[i] <- relevantObjectsTested$unexpected2.rot[i]
          }
        } else {
          if(relevantObjectsTested$encodingCondition[i] == 'expected'){
            relevantObjectsTested$encodingLocation[i] <- relevantObjectsTested$expected3[i]
            relevantObjectsTested$encodingRotation[i] <- relevantObjectsTested$expected3.rot[i]
          } else {
            relevantObjectsTested$encodingLocation[i] <- relevantObjectsTested$unexpected3[i]
            relevantObjectsTested$encodingRotation[i] <- relevantObjectsTested$unexpected3.rot[i]
          }
        }
      }
      count <- count  + 1
    }
    unUsedLocations                          <- setdiff(possibleLocations, relevantObjectsTested$encodingLocation)
    irrelevantObjectsTested$encodingLocation <- sample(unUsedLocations, numIrrelevantObjectsTested)
    
    # Format for VR
    # name spawnPoint yRotations yAdjustments
    # Format for recognition
    # objectNames, presented, room, relevancy, encodingCondition, retrievalCondition, firstLocation, secondLocation, thirdLocation
    stimList          <- data.frame(name = c(as.character(relevantObjectsTested$objectName),as.character(irrelevantObjectsTested$objectName)),
                                    spawnPoint = c(as.character(relevantObjectsTested$encodingLocation),as.character(irrelevantObjectsTested$encodingLocation)),
                                    yRotations = c(relevantObjectsTested$encodingRotation, rep(0,4)),
                                    yAdjustments = c(relevantObjectsTested$adjust.y, irrelevantObjectsTested$adjust.y))
    stimLists <- rbind(stimLists, cbind(rep(subject, 12), stimList))
    write.table(stimList,  file = paste('output/stimList_',as.character(subject), '.txt', sep = ''), sep = "\t",row.names = F, col.names = F, quote = F)
    
    stimListRetrieval <- data.frame(name      = c(as.character(relevantObjectsTested$objectName),as.character(irrelevantObjectsTested$objectName), setdiff(allNames, c(as.character(relevantObjectsTested$objectName),as.character(irrelevantObjectsTested$objectName)))),
                                    presented = c(rep(1, numRelevantObjectsTested + numIrrelevantObjectsTested), rep(0, numFoils)),
                                    room      = rep('kitchen', numRelevantObjectsTested + numIrrelevantObjectsTested + numFoils),
                                    relevancy = c(as.character(relevantObjectsTested$relevancy), as.character(irrelevantObjectsTested$relevancy), as.character(data[data$objectName %in% setdiff(allNames, c(as.character(relevantObjectsTested$objectName),as.character(irrelevantObjectsTested$objectName))),'relevancy'])))
    stimListRetrieval$encodingCondition  <- ''
    stimListRetrieval$retrievalCondition <- ''
    stimListRetrieval$firstLocation      <- ''
    stimListRetrieval$secondLocation     <- ''
    stimListRetrieval$thirdLocation      <- ''
    count1                               <- 1
    count2                               <- 1
    count3                               <- 1
    expectednessRetrieval1               <- sample(expectednessRetrieval)
    expectednessRetrieval2               <- sample(expectednessRetrieval)
    expectednessRetrieval3               <- sample(expectednessRetrieval)
    for(y in 1:dim(stimListRetrieval)[1]){
      if(stimListRetrieval$present[y] == 1){
        if(stimListRetrieval$relevancy[y] == 'relevant'){
          stimListRetrieval$encodingCondition[y]    <- as.character(relevantObjectsTested$encodingCondition[y])
          stimListRetrieval$firstLocation[y]        <- as.character(relevantObjectsTested$encodingLocation[y])
          if(stimListRetrieval$encodingCondition[y] == 'expected'){
            stimListRetrieval$retrievalCondition[y] <-expectednessRetrieval1[count1]
            count1                                  <- count1 + 1
            locations <- c(as.character(data[data$objectName == stimListRetrieval$name[y], 'expected1']), as.character(data[data$objectName == stimListRetrieval$name[y], 'expected2']),as.character(data[data$objectName == stimListRetrieval$name[y], 'expected3']))
            notUsed   <- setdiff(locations, stimListRetrieval$firstLocation[y]) 
            stimListRetrieval$secondLocation[y] <- notUsed[1]
            stimListRetrieval$thirdLocation[y]  <- notUsed[2]
          } else {
            stimListRetrieval$retrievalCondition[y] <-expectednessRetrieval2[count2]
            count2                                  <- count2 + 1
            locations <- c(as.character(data[data$objectName == stimListRetrieval$name[y], 'unexpected1']), as.character(data[data$objectName == stimListRetrieval$name[y], 'unexpected2']),as.character(data[data$objectName == stimListRetrieval$name[y], 'unexpected3']))
            notUsed   <- setdiff(locations, stimListRetrieval$firstLocation[y]) 
            stimListRetrieval$secondLocation[y] <- notUsed[1]
            stimListRetrieval$thirdLocation[y]  <- notUsed[2]
          }
        } else {
          stimListRetrieval$encodingCondition[y]    <- 'none'
          stimListRetrieval$retrievalCondition[y]   <- 'none'
          stimListRetrieval$firstLocation[y]        <- as.character(irrelevantObjectsTested[irrelevantObjectsTested$objectName == stimListRetrieval$name[y], 'encodingLocation'])
        }
        
      } else {
        stimListRetrieval$encodingCondition[y]    <- 'none'
        if(stimListRetrieval$relevancy[y] == 'relevant'){
          stimListRetrieval$retrievalCondition[y] <-expectednessRetrieval3[count3]
          count3                                  <- count3 + 1
          if(stimListRetrieval$retrievalCondition[y] == 'expected'){
            stimListRetrieval$firstLocation[y]  <- as.character(data[data$objectName == stimListRetrieval$name[y], 'expected1'])
            stimListRetrieval$secondLocation[y] <- as.character(data[data$objectName == stimListRetrieval$name[y], 'expected2'])
            stimListRetrieval$thirdLocation[y]  <- as.character(data[data$objectName == stimListRetrieval$name[y], 'expected3'])
          } else {
            stimListRetrieval$firstLocation[y]  <- as.character(data[data$objectName == stimListRetrieval$name[y], 'unexpected1'])
            stimListRetrieval$secondLocation[y] <- as.character(data[data$objectName == stimListRetrieval$name[y], 'unexpected2'])
            stimListRetrieval$thirdLocation[y]  <- as.character(data[data$objectName == stimListRetrieval$name[y], 'unexpected3'])
          }
        } else {
          stimListRetrieval$retrievalCondition[y] <- 'none'
        }
      }
    }
    stimListRetrieval$firstLocation <- as.numeric(gsub("[^0-9]", "", stimListRetrieval$firstLocation))
    stimListRetrieval$secondLocation <- as.numeric(gsub("[^0-9]", "", stimListRetrieval$secondLocation))
    stimListRetrieval$thirdLocation <- as.numeric(gsub("[^0-9]", "", stimListRetrieval$thirdLocation))
    stimListRetrieval[which(stimListRetrieval$retrievalCondition == 'none' & stimListRetrieval$present == 0),'firstLocation'] <- sample(setdiff(1:20, as.numeric(gsub("[^0-9]", "", stimListRetrieval$firstLocation))), 4)
    for(z in 1:(numRelevantObjects + numIrrelevantObjects)){
      if(stimListRetrieval$retrievalCondition[z] == 'none' & stimListRetrieval$presented[z] == 0){
        locations <- sample(setdiff(1:12, stimListRetrieval$firstLocation[z]), 2)
        stimListRetrieval$secondLocation[z] <- locations[1]
        stimListRetrieval$thirdLocation[z] <- locations[2]
      }
      if(stimListRetrieval$retrievalCondition[z] == 'none' & stimListRetrieval$presented[z] == 1){
        locations <- sample(setdiff(1:12, stimListRetrieval$firstLocation[z]), 2)
        stimListRetrieval$secondLocation[z] <- locations[1]
        stimListRetrieval$thirdLocation[z] <- locations[2]
      }
    } 
    write.table(stimListRetrieval,  file = paste('output/stimListRetrieval_',as.character(subject), '.txt', sep = ''), sep = "\t",row.names = F, col.names = F, quote = F)
  }
}

