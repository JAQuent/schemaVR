exp3_sets <- ddply(dataSchemaVR3_AFC, c('subNum', 'setNum'), summarise, N = length(subNum))

table(exp3_sets$setNum)


exp4_sets <- ddply(dataSchemaVR4_AFC, c('subNum', 'setNum'), summarise, N = length(subNum))

table(exp3_sets$setNum)
