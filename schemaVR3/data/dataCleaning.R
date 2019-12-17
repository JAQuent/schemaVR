# Script for cleaning and preparing the data from schemaVR3
# Version 1.0
# Date:  16/10/2019
# Author: Joern Alexander Quent
# /* 
# ----------------------------- Cleaning and preparing ---------------------------
# */
# Loading data
load("U:/Projects/schemaVR/schemaVR3/data/dataSchemaVR3.RData")

# TO2JKJ said no memory for fruitbowl instead of guess
dataSchemaVR3[dataSchemaVR3$subNum == 'TO2JKJ' & dataSchemaVR3$objNam == 'bowl of fruits', 'recallMemory'] <- 0
dataSchemaVR3[dataSchemaVR3$subNum == 'TO2JKJ' & dataSchemaVR3$objNam == 'mixer', 'recallMemory']          <- 0
# Toy that was presented before the helmet should become NA
# OW0NEB very poor recall performance
# 3IHX8R meant to press 2 instead of 1 for toy
dataSchemaVR3[dataSchemaVR3$subNum == '3IHX8R' & dataSchemaVR3$objNam == 'toy', 'resAFC'] <- 2
# GRBBT1 said remember to towels
dataSchemaVR3[dataSchemaVR3$subNum == 'GRBBT1' & dataSchemaVR3$objNam == 'towels', 'recallMemory'] <- 1

# Decided to exclude SJ3ENA because of malfunction of equipment
dataSchemaVR3 <- dataSchemaVR3[dataSchemaVR3$subNum != 'SJ3ENA', ]

dataSchemaVR3$setNum <- as.factor(dataSchemaVR3$setNum )

# Subsetting data
dataSchemaVR3_AFC    <- subset(dataSchemaVR3, dataSchemaVR3$resCon != '0')
dataSchemaVR3_recall <- subset(dataSchemaVR3, dataSchemaVR3$recallMemory != '0' & !is.na(dataSchemaVR3$recallMemory))

# /* 
# ----------------------------- Cleaning and preparing ---------------------------
# */
save.image('data/dataSchemaVR3_cleaned.RData')