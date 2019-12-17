# Script to find next locations in schemaVR3
# Version 1.0
# Date:  16/09/2019
# Author: Joern Alexander Quent
# Loading data
load("U:/Projects/schemaVR/schemaVR3/data/dataSchemaVR3_cleaned.RData")
normativeData <- read.table("U:/Projects/schemaVR/schemaVR3/data/normativeData.txt", 
                            header = TRUE, 
                            sep = "\t")

# To make the code work below, I have to replace the object names
normativeData$objectNames <- as.character(normativeData$objectNames)
dataSchemaVR3$objNam      <- as.character(dataSchemaVR3$objNam)
normativeData$objectNames[normativeData$objectNames == 'fruit bowl'] <- 'bowl of fruits'

# Getting normative expectancy for closest location
dataSchemaVR3$closestObjLocNorm <- NA
dataSchemaVR3$objLocTargetNorm  <- NA
# Add normative location rating
for(i in 1:dim(dataSchemaVR3)[1]){
  if(!is.na(dataSchemaVR3$closestLoc[i])){
    # Necessary to use if statement because some values are NA and cannot be used to index
    dataSchemaVR3$closestObjLocNorm[i] <- normativeData[normativeData$objectNames == as.character(dataSchemaVR3$objNam[i]), 
                                                        paste("loc", dataSchemaVR3$closestLoc[i], sep = "")] 
  }
  dataSchemaVR3$objLocTargetNorm[i] <- normativeData[normativeData$objectNames == as.character(dataSchemaVR3$objNam[i]), 
                                                     paste("loc", dataSchemaVR3$targetLocation[i], sep = "")]
}

# /* 
# ----------------------------- Saving ---------------------------
# */
save.image('U:/Projects/schemaVR/schemaVR3/data/schemaVR3_closestLocation.RData')
