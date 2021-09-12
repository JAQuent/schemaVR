# Calculate spawnPoints for schemaVR2


dataAFC <- rbindlist(lapply(paste('data/retrievalTask_', as.character(20),'.dat', sep = ''), fread))
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

dataAFC$objName    <- factor(dataAFC$objNum, labels = objNames)

encodingLocations        <- fread("data/inputFileEncoding2.txt")
names(encodingLocations) <- c('objName', 
                              'xRotation', 
                              'yRotation', 
                              'zRotation', 
                              'xPosition', 
                              'yPosition', 
                              'zPosition')


a <- dataAFC$targetLocation[1:20]
b <- as.character(dataAFC$objName[1:20])
encodingLocations$objName <- as.character(encodingLocations$objName)
encodingLocations[which(encodingLocations$objName == 'kitchenRoll')]$objName    <-'kitchen roll'
encodingLocations[which(encodingLocations$objName == 'fruitBowl')]$objName      <-'bowl of fruits'
encodingLocations[which(encodingLocations$objName == 'teaPot')]$objName         <-'tea pot'
encodingLocations[which(encodingLocations$objName == 'glassContainer')]$objName <- 'glass jug'
encodingLocations[which(encodingLocations$objName == 'towel')]$objName          <- 'towels'
encodingLocations[which(encodingLocations$objName == 'bookPile')]$objName       <- 'pile of books'

a <- a[order(b)]
encodingLocations <- encodingLocations[order(encodingLocations$objName)]
b <- sort(b)

b <- b[order(a)]
encodingLocations <- encodingLocations[order(a)]
a <- sort(a)

shitfs <- euclideanDistance3D(encodingLocations$xPosition, 
                              encodingLocations$yPosition,
                              encodingLocations$zPosition,
                              spawnPoints$x,
                              spawnPoints$y,
                              spawnPoints$z)
mean(shitfs)
sd(shitfs)
hist(shitfs)
# Discuss these results

spawnPoints3 <- data.frame(point = factor(1:20),
                           x = encodingLocations$xPosition,
                           y = encodingLocations$yPosition,
                           z = encodingLocations$zPosition)

write.table(spawnPoints3, 
            file = 'spawnPoints3.txt', 
            col.names = TRUE, 
            row.names = FALSE, 
            quote = FALSE)
