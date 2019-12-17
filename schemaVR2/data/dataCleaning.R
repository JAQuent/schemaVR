# Script for cleaning and preparing the data from schemaVR2
# Version 1.0
# Date:  11/10/2019
# Author: Joern Alexander Quent
# /* 
# ----------------------------- Cleaning and preparing ---------------------------
# */
# Loading data
load("U:/Projects/schemaVR/schemaVR2/data/dataSchemaVR2.RData")

# Delete as unnessacary
rm(dataSchemaVR2_sub) 

#Add (non-)kitchen object factor to data
dataSchemaVR2$expectedInKitchen                                      <- 'non-kitchen'
dataSchemaVR2[which(dataSchemaVR2$objNum < 13), 'expectedInKitchen'] <- 'kitchen'

# Notes and decision on participants:
# Exclude participants because they did the  wrong objLocTargetRating
dataSchemaVR2 <- subset(dataSchemaVR2, subNum != 'AX0G39' & 
                          subNum != 'RYMC43' &
                          subNum != 'FC8VI8' &
                          subNum != '2KKJ7Q' &
                          subNum != 'QAIRIV')
# Participant OJ26QA, 6EZGH1 and NYI7LD: Rated object 16 at location 1 instead of 14. This value is therefore missing. 
# Also Foil2 for umbrella was not saved.
# Participant 6EZGH1: Check recall microwave because it was correct (it is). The 2nd rating was 100 not 0.
dataSchemaVR2[which(dataSchemaVR2$subNum == '6EZGH1' & dataSchemaVR2$objNum == 3), 'generalRatingPost'] <- 100

# Response given for judgement (0 = no memory, 1 = remember, 2 = familiar, 3 = guess)
dataSchemaVR2$recallMemory[which(dataSchemaVR2$recallMemory == -1)] <- NA # Code missing value

# Subsetting data
dataSchemaVR2_recall  <- subset(dataSchemaVR2, dataSchemaVR2$recallMemory != 0 | !is.na(dataSchemaVR2$recallMemory))
dataSchemaVR2_AFC     <- subset(dataSchemaVR2, resCon != 0)

# /* 
# ----------------------------- Cleaning and preparing ---------------------------
# */
save.image('data/dataSchemaVR2_cleaned.RData')