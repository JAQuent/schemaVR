setwd("U:/Projects/Exp1/variousStuff/randomization")

objectNames <- c('microwave',
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

numerus <- c(rep('s', 11), rep('p', 2), rep('s', 7))

nObject        <- 20
nLocation      <- 400
n              <- 10
objectQuestion <- c()

for(i in 1:nObject){
  if(numerus[i] == 's'){
    objectQuestion[i] <- paste('How expected is this', objectNames[i], 'in a kitchen?')
  } else {
    objectQuestion[i] <- paste('How expected are these', objectNames[i], 'in a kitchen?')
  }
}

objectFrame <- data.frame(type = rep('object', 20), fileName = 1:20, question = objectQuestion)


locations <- paste(as.character(rep(1:20, each = 20)), "_", as.character(rep(1:20, 20)), sep = '')


for(i in 1:n){
  shuffle <- sample(1:nObject)
  locationFrame <- data.frame(type = rep('location', 400), fileName = sample(locations), question = rep('empty', 400), stringsAsFactors = FALSE)
  for(j in 1:nLocation){
    if(numerus[as.numeric(unlist(strsplit(locationFrame$fileName[j], '_'))[1])] == 's'){
      locationFrame[j, 'question'] <- paste('How expected is this', objectNames[as.numeric(unlist(strsplit(locationFrame$fileName[j], '_'))[1])], 'in that location?')
    } else {
      locationFrame[j, 'question'] <- paste('How expected are these', objectNames[as.numeric(unlist(strsplit(locationFrame$fileName[j], '_'))[1])], 'in that location?')
    }
  }
  write.table(rbind(locationFrame, objectFrame[shuffle,]),  file = paste('ratingLogFiles/questions',as.character(i), '.txt', sep = ''), quote = FALSE, col.names = FALSE, row.names = FALSE, sep = '\t')
}
