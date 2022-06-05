# Prepare
subNum          <- 1:6
N               <- length(subNum)
locationRatings <- array(data = NA, dim = c(20, 20, N))
objectRatings   <- matrix(NA, 20, N)

# Sequently loading data
for(i in 1:N){
  locationRatings[,,i] <- matrix(scan(paste('normativeData/locationRatings_', as.character(subNum[i]) ,'.dat', sep = '')), byrow = TRUE, ncol = 20)
  objectRatings[,i]    <- scan(paste('normativeData/objectRatings_', as.character(subNum[i]) ,'.dat', sep = ''))
}

objNames <- c('microwave',
              'kitchen roll',
              'saucepan', 
              'toaster',
              'fruit bowl',
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

# Calculate metrics
averageLocationRatings <- apply(locationRatings, 1:2, mean)
averageObjectRatings   <- apply(objectRatings, 1, mean)

normativeData <- data.frame(objNames = objNames,
                            general = averageObjectRatings,
                            loc1 = averageLocationRatings[1,],
                            loc2 = averageLocationRatings[2,],
                            loc3 = averageLocationRatings[3,],
                            loc4 = averageLocationRatings[4,],
                            loc5 = averageLocationRatings[5,],
                            loc6 = averageLocationRatings[6,],
                            loc7 = averageLocationRatings[7,],
                            loc8 = averageLocationRatings[8,],
                            loc9 = averageLocationRatings[9,],
                            loc10 = averageLocationRatings[10,],
                            loc11 = averageLocationRatings[11,],
                            loc12 = averageLocationRatings[12,],
                            loc13 = averageLocationRatings[13,],
                            loc14 = averageLocationRatings[14,],
                            loc15 = averageLocationRatings[15,],
                            loc16 = averageLocationRatings[16,],
                            loc17 = averageLocationRatings[17,],
                            loc18 = averageLocationRatings[18,],
                            loc19 = averageLocationRatings[19,],
                            loc20 = averageLocationRatings[20,])

write.table(normativeData, 
            file = 'normativeData.txt', 
            col.names = TRUE, 
            row.names = FALSE, 
            quote = FALSE,
            sep = "\t")