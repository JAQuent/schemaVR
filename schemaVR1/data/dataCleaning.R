# Script for cleaning and preparing the data from schemaVR1
# Version 1.0
# Date:  11/10/2019
# Author: Joern Alexander Quent
# /* 
# ----------------------------- Cleaning and preparing ---------------------------
# */
# Loading data
load("U:/Projects/schemaVR/schemaVR1/data/dataSchemaVR1.RData")

# Delete as unnessacary
rm(dataSchemaVR1_sub) 

#Add (non-)kitchen object factor to data
dataSchemaVR1$expectedInKitchen                                      <- 'non-kitchen'
dataSchemaVR1[which(dataSchemaVR1$objNum < 13), 'expectedInKitchen'] <- 'kitchen'

# Notes and decision regarding participants:
# Participant FTSIQ0: Sligthly different instructions for no-memory trial -> exlcude.
dataSchemaVR1 <- subset(dataSchemaVR1, subNum != 'FTSIQ0')
# Participant FTSIQ0 to AWQ44N: Had no familiarisation phase
# Participant JMK85L and 306HM8: Second exposure with stimulus -> include.
# Participant ENEPJG: No memory trials on trial 1 and 2 because I pressed those accidently -> changed.
dataSchemaVR1[which(dataSchemaVR1$subNum == 'ENEPJG' & (dataSchemaVR1$recallTrial == 1 | dataSchemaVR1$recallTrial == 2)), 'recallNoMemory'] <- 1
# Participant RZ0GTE: Problems with data and from towel -> no anormalities.
# Participant KHNHVI Had a longer delay (1.5 min)

# Subsetting data
dataSchemaVR1_recall <- subset(dataSchemaVR1, dataSchemaVR1$recallNoMemory == 0)
dataSchemaVR1_AFC    <- subset(dataSchemaVR1, dataSchemaVR1$resCon != 1)

# /* 
# ----------------------------- Cleaning and preparing ---------------------------
# */
save.image('data/dataSchemaVR1_cleaned.RData')