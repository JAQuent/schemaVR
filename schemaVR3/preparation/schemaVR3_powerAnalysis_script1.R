####################################
# Loading and excluding data
# Loading schemaVR2
setwd("U:/Projects/schemaVR/schemaVR3/preparation")
load("U:/Projects/schemaVR/report_firstYear/report/data/exp2Data.RData")
dataSchemaVR2 <- combData
rm(combData)

# Loading normative data
normativeData <- read.csv('ratingsForSets.csv')

#Add (non-)kitchen object factor to data
dataSchemaVR2$expectedInKitchen                                      <- 'non-kitchen'
dataSchemaVR2[which(dataSchemaVR2$objNum < 13), 'expectedInKitchen'] <- 'kitchen'

# Notes and decision on participants:
# Participant #20 to #24: Exclude participants because they did the  wrong objLocTargetRating
dataSchemaVR2 <- subset(dataSchemaVR2, subNum >= 25)
# Participant #20 to #27: Foil2 for umbrella was not saved.
# Participant #25 to #27: Rated object 16 at location 1 instead of 14. This value is therefore missing.
# Participant #22: Seen objects twice but is excluded anyway. 
# Participant #26: Check recall microwave because it was correct (it is). The 2nd rating was 100 not 0.
dataSchemaVR2[which(dataSchemaVR2$subNum == 26 & dataSchemaVR2$objNum == 3), 'generalRatingPost'] <- 100

# Response given for judgement (0 = no memory, 1 = remember, 2 = familiar, 3 = guess)
dataSchemaVR2$recallMemory[which(dataSchemaVR2$recallMemory == -1)] <- NA # Code missing value

####################################
# Aggregating
corrData <- ddply(dataSchemaVR2,
                  c('objNum', 'objName'),
                  summarise,
                  ratingSD           = sd(objLocTargetRating, na.rm = TRUE),
                  objLocTargetRating = mean(objLocTargetRating, na.rm = TRUE),
                  AFCNoMemory        = table(resCon)[1],
                  AFCRecollection    = table(resCon)[2],
                  AFCFamiliarity     = table(resCon)[3],
                  AFCGuess           = table(resCon)[4],
                  AFCN               = sum(table(resCon)),
                  AFCRec_per         = (AFCRecollection/AFCN),
                  AFCFam_per         = (AFCFamiliarity/AFCN),
                  AFCFam_per_ind     = (AFCFamiliarity/(AFCN - AFCRecollection)),
                  AFCGuess_per       = (AFCGuess/(AFCN - AFCRecollection - AFCFamiliarity)))

cor.test(corrData$objLocTargetRating, corrData$AFCRecollection)

####################################
# Calculating the best linear fit for recollection
y_rec       <- corrData$AFCRec_per
X           <- matrix(c(corrData$objLocTargetRating, rep(1, 20)), ncol = 2)
# Performing a matrix multiplication of the pseudoinverse of X and y
B_rec       <- pinv(X) %*% y_rec
resid_rec   <- y_rec - X %*% B_rec
residSD_rec <- sd(resid_rec)

# Calculating the best linear fit for familairity
y_fam       <- corrData$AFCFam_per_ind
# Performing a matrix multiplication of the pseudoinverse of X and y
B_fam       <- pinv(X) %*% y_fam
resid_fam   <- y_fam - X %*% B_fam
residSD_fam <- sd(resid_fam)

####################################
# William's test following Rik
williamsTest <- function(x, y1, y2){
  # Calculate correlation and peform Fisher transformation
  z1  <- atanh(cor(x, y1))
  z2  <- atanh(cor(x, y2))
  z3  <- atanh(cor(y1, y2))
  df  <- length(x) - 3
  
  # Calculate test statistic
  rm2 <- (z1^2 + z2^2)/2
  f   <- (1 - z3) / (2 * (1 - rm2))
  h   <- (1 - f * rm2) / (1 - rm2)
  Z   <- (z1 - z2) * sqrt(df / (2 * ( 1 - z3)* h))
  return(c(Z, df))
}

# Preparation for simulation
set.seed(16)
nSims                <- 10000
sets                 <- 1:5
nObj                 <- 20
nSubs                <- 5
rValues_rec <- matrix(0, ncol = 1, nrow = nSims)
rValues_fam  <- matrix(0, ncol = 1, nrow = nSims)  
tValues              <- matrix(0, ncol = 1, nrow = nSims)  

# Exclude non unique

# Simulation
for(i in 1:nSims){
 # Simulation loop 
  meanExpectancy          <- c()
  probabilityRecollection <- c()
  probabilityFamiliarity  <- c()
 for(j in sets){
   # Set loop
   # Calculate expectancy
   meanExpectancyTemp         <- normativeData[normativeData$set == j, 'expectancy']
   
   # Calculate prediction for new data points
   # Recollection
   prediction                  <- matrix(matrix(c(meanExpectancyTemp, rep(1, nObj)), ncol = 2) %*% B_rec, ncol = nSubs, nrow = nObj)
   predictionPlusNoise         <- prediction + matrix(rnorm(nSubs * nObj), ncol = nSubs, nrow = nObj)*residSD_rec
   probabilityRecollectionTemp <- rowMeans(matrix(runif(nSubs * nObj, 0, 1), ncol = nSubs, nrow = nObj) <= predictionPlusNoise)
   
   # Familar
   prediction                  <- matrix(matrix(c(meanExpectancyTemp, rep(1, nObj)), ncol = 2) %*% B_fam, ncol = nSubs, nrow = nObj)
   predictionPlusNoise         <- prediction + matrix(rnorm(nSubs * nObj), ncol = nSubs, nrow = nObj)*residSD_fam
   probabilityFamiliarityTemp  <- rowMeans(matrix(runif(nSubs * nObj, 0, 1), ncol = nSubs, nrow = nObj) <= predictionPlusNoise)
   
   # Concatenate vectors
   meanExpectancy          <- c(meanExpectancy, meanExpectancyTemp)
   probabilityRecollection <- c(probabilityRecollection, probabilityRecollectionTemp)
   probabilityFamiliarity  <- c(probabilityFamiliarity, probabilityFamiliarityTemp)
   
 }
  # Calculate results for this run
  rValues_rec[i] <- cor(meanExpectancy, probabilityRecollection)
  rValues_fam[i] <- cor(meanExpectancy, probabilityFamiliarity)
  tValues[i]     <- williamsTest(meanExpectancy, probabilityRecollection, probabilityFamiliarity)[1]
}

####################################
# Calculate power
# Recollection
tRec     <- rValues_rec * sqrt((100 - 2)/ (1 - rValues_rec^2))
pRec     <- pt(tRec, 100 - 2)
powerRec <- sum(pRec < 0.05)/nSims

# Familarity
tFam     <- rValues_fam * sqrt((100 - 2)/ (1 - rValues_fam^2))
pFam     <- pt(tFam, 100 - 2)
powerFam <- sum(pFam < 0.05)/nSims

# Williams
pWill     <- pt(tValues, 100 - 2)
powerWill <- sum(pWill < 0.05)/nSims
