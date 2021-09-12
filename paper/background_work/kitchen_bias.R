library(BayesFactor)
library(assortedRFunctions)

dataSchemaVR1$kitchen     <- ifelse(dataSchemaVR1$objNum <= 12, 'kitchen', 'non-kitchen')
dataSchemaVR1$sExp        <- dataSchemaVR1$objLocTargetRating/sd_value_recall


kitchen_expectancy     <- dataSchemaVR1[dataSchemaVR1$kitchen == 'kitchen', 'sExp']
non_kitchen_expectancy <- dataSchemaVR1[dataSchemaVR1$kitchen == 'non-kitchen', 'sExp']

ttestBF(abs(0 - kitchen_expectancy), abs(0 - non_kitchen_expectancy))

mean(abs(0 - kitchen_expectancy))
mean(abs(0 - non_kitchen_expectancy))


dataSchemaVR2$kitchen     <- ifelse(dataSchemaVR2$objNum <= 12, 'kitchen', 'non-kitchen')
dataSchemaVR2$sExp        <- dataSchemaVR2$objLocTargetRating/sd_value_recall


kitchen_expectancy     <- dataSchemaVR2[dataSchemaVR2$kitchen == 'kitchen', 'sExp']
non_kitchen_expectancy <- dataSchemaVR2[dataSchemaVR2$kitchen == 'non-kitchen', 'sExp']

ttestBF(abs(0 - kitchen_expectancy), abs(0 - non_kitchen_expectancy))

mean(abs(0 - kitchen_expectancy))
mean(abs(0 - non_kitchen_expectancy))


dataSchemaVR3$kitchen     <- ifelse(dataSchemaVR3$objNum <= 12, 'kitchen', 'non-kitchen')
dataSchemaVR3$sExp        <- dataSchemaVR3$objLocTargetRating/sd_value_recall


kitchen_expectancy     <- dataSchemaVR3[dataSchemaVR3$kitchen == 'kitchen', 'sExp']
non_kitchen_expectancy <- dataSchemaVR3[dataSchemaVR3$kitchen == 'non-kitchen', 'sExp']

ttestBF(abs(0 - kitchen_expectancy), abs(0 - non_kitchen_expectancy))

mean(abs(0 - kitchen_expectancy))
mean(abs(0 - non_kitchen_expectancy))



# Normative for Exp1 
# Finding the expectancy of the closest location for an object
dataSchemaVR1$objLocTargetNorm <- NA
# Add normative location rating
for(i in 1:dim(dataSchemaVR1)[1]){
  if(!is.na(dataSchemaVR1$targetLocation[i])){
    # Necessary to use if statement because some values are NA and cannot be used to index
    dataSchemaVR1$objLocTargetNorm[i] <- dataSchemaVR1[i, paste("loc", dataSchemaVR1$targetLocation[i], sep = "")] 
  }
}

dataSchemaVR1$kitchen     <- ifelse(dataSchemaVR1$objNum <= 12, 'kitchen', 'non-kitchen')

kitchen_expectancy     <- dataSchemaVR1[dataSchemaVR1$kitchen == 'kitchen' & dataSchemaVR1$subNum == 'VIXTL5', 'objLocTargetNorm']
non_kitchen_expectancy <- dataSchemaVR1[dataSchemaVR1$kitchen == 'non-kitchen' & dataSchemaVR1$subNum == 'VIXTL5', 'objLocTargetNorm']

ttestBF(abs(0 - kitchen_expectancy), abs(0 - non_kitchen_expectancy))

mean_SD_str2(abs(0 - kitchen_expectancy), 1)
mean_SD_str2(abs(0 - non_kitchen_expectancy), 1)




# Normative for Exp 2
dataSchemaVR2$kitchen  <- ifelse(dataSchemaVR2$objNum <= 12, 'kitchen', 'non-kitchen')
kitchen_expectancy     <- dataSchemaVR2[dataSchemaVR2$kitchen == 'kitchen' & dataSchemaVR2$subNum == 'D5CAZY', 'objLocTargetNorm']
non_kitchen_expectancy <- dataSchemaVR2[dataSchemaVR2$kitchen == 'non-kitchen' & dataSchemaVR2$subNum == 'D5CAZY', 'objLocTargetNorm']

ttestBF(abs(0 - kitchen_expectancy), abs(0 - non_kitchen_expectancy))

mean_SD_str2(abs(0 - kitchen_expectancy), 1)
mean_SD_str2(abs(0 - non_kitchen_expectancy), 1)

# Normative for Exp 4
load("C:/Users/aq01/Desktop/schemaVR/schemaVR4/data/schemaVR4_closestLocation.RData")

set_object <- ddply(dataSchemaVR4, c('setNum', 'objNum'), summarise, objLocTargetNorm = objLocTargetNorm[1])

set_object$kitchen     <- ifelse(set_object$objNum <= 12, 'kitchen', 'non-kitchen')

kitchen_expectancy     <- set_object[set_object$kitchen == 'kitchen', 'objLocTargetNorm']
non_kitchen_expectancy <- set_object[set_object$kitchen == 'non-kitchen', 'objLocTargetNorm']

ttestBF(abs(0 - kitchen_expectancy), abs(0 - non_kitchen_expectancy))

mean_SD_str2(abs(0 - kitchen_expectancy), 1)
mean_SD_str2(abs(0 - non_kitchen_expectancy), 1)