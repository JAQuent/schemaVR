# Script for cleaning and preparing the data from schemaVR4
# Version 1.1
# Date:  28/06/2021
# Author: Joern Alexander Quent
# /* 
# ----------------------------- Cleaning and preparing ---------------------------
# */
# Clear workspace
rm(list = ls())

# Path to parent folder schemaVR
path2parent <- "D:/Alex/Laptop/Desktop/schemaVR" # This need to be changed to run this document

# Loading data
load(paste0(path2parent, "/schemaVR4/data/dataSchemaVR4.RData"))

# 28I8OU mug was a no memory not a guess. 
dataSchemaVR4[dataSchemaVR4$subNum == '28I8OU' & dataSchemaVR4$objNam == 'mug', 'recallMemory'] <- 0

# 5MJ39C Had double the data in their data file. The other data is from SDSAO7 (day 1). Luckily both have the same set. 
# That means for 5MJ39C only the recall data is there but not the 
# correction. I used the correction value from another person on the same date, which should work.

#mean(dataSchemaVR4[dataSchemaVR4$subNum == '5MJ39C', 'accRecall']) Mean recall seems to be okay with 0.25 (overall mean is around 0.3)

# VVIX6F actually said no memory to glass jar
dataSchemaVR4[dataSchemaVR4$subNum == 'VVIX6F' & dataSchemaVR4$objNam == 'glass jug', 'recallMemory'] <- 0

# I also had to delete the first row in RHSRDV (no further action necessary)

# 6JZSA4 trial before umbrella (jar) was R in case I forgot to press
dataSchemaVR4[dataSchemaVR4$subNum == '6JZSA4' & dataSchemaVR4$objNam == 'glass jug', 'recallMemory'] <- 1

# 8W4FDN I clicked too early but is fine


# Make set a factor
dataSchemaVR4$setNum <- as.factor(dataSchemaVR4$setNum)

# Subsetting data
dataSchemaVR4_AFC    <- subset(dataSchemaVR4, dataSchemaVR4$resCon != '0')
dataSchemaVR4_recall <- subset(dataSchemaVR4, dataSchemaVR4$recallMemory != '0' & !is.na(dataSchemaVR4$recallMemory))

# /* 
# ----------------------------- Cleaning and preparing ---------------------------
# */
save.image(paste0(path2parent, "/schemaVR4/data/dataSchemaVR4_cleaned.RData"))
