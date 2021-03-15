# Script to find next locations in schemaVR4
# Version 1.0
# Date:  15/03/2021
# Author: Joern Alexander Quent

# Path to parent folder schemaVR
path2parent <- "C:/Users/aq01/Desktop/schemaVR" # This need to be changed to run this document

# Loading data
load(paste0(path2parent, "/schemaVR4/data/dataSchemaVR4_cleaned.RData"))

# Get the normative data
normativeData <- read.table(paste0(path2parent, "/schemaVR4/data/normativeData.txt"), 
                            header = TRUE, 
                            sep = "\t")

# To make the code work below, I have to replace the object names
normativeData$objectNames <- as.character(normativeData$objectNames)
dataSchemaVR4$objNam      <- as.character(dataSchemaVR4$objNam)
normativeData$objectNames[normativeData$objectNames == 'fruit bowl'] <- 'bowl of fruits'

# Getting normative expectancy for closest location
dataSchemaVR4$closestObjLocNorm <- NA
dataSchemaVR4$objLocTargetNorm  <- NA
# Add normative location rating
for(i in 1:dim(dataSchemaVR4)[1]){
  if(!is.na(dataSchemaVR4$closestLoc[i])){
    # Necessary to use if statement because some values are NA and cannot be used to index
    dataSchemaVR4$closestObjLocNorm[i] <- normativeData[normativeData$objectNames == as.character(dataSchemaVR4$objNam[i]), 
                                                        paste("loc", dataSchemaVR4$closestLoc[i], sep = "")] 
  }
  dataSchemaVR4$objLocTargetNorm[i] <- normativeData[normativeData$objectNames == as.character(dataSchemaVR4$objNam[i]), 
                                                     paste("loc", dataSchemaVR4$targetLocation[i], sep = "")]
}

# /* 
# ----------------------------- Saving ---------------------------
# */
save.image(paste0(path2parent, "/schemaVR4/data/schemaVR4_closestLocation.RData"))
