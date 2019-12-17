rikData <- data.frame(subNum = dataSchemaVR2$subNum,
                      objNum = dataSchemaVR2$objName,
                      kitchen = dataSchemaVR2$expectedInKitchen,
                      expectancy = dataSchemaVR2$objLocTargetRating,
                      recollection = dataSchemaVR2$recollection3AFC,
                      familiar     =  dataSchemaVR2$familiarity3AFC,
                      guess        = dataSchemaVR2$guess3AFC)

rikData$noMemory <- 0
rikData$noMemory[rikData$recollection == 'not recollected' & 
                   rikData$familiar == 'not familiar' & 
                   rikData$guess == 'not guessed'] <- 1

write.csv(rikData, 
          'preparation/rikData.csv',
          row.names = FALSE)


schemaVR2_table4 <- ddply(dataSchemaVR2, 
                          c('objNum', 'objName'), 
                          summarise,
                          objLocTargetRating = mean(objLocTargetRating, na.rm = TRUE),
                          RecallNoMemory     = table(recallMemory)[1],
                          RecallRecollection = table(recallMemory)[2],
                          RecallFamiliarity  = table(recallMemory)[3],
                          RecallGuess        = table(recallMemory)[4],
                          RecallN            = sum(table(recallMemory)),
                          RecallRec_per      = (RecallRecollection/RecallN)*100,
                          RecallFam_per      = (RecallFamiliarity/RecallN)*100,
                          RecallFam_per_ind  = (RecallFamiliarity/(RecallN - RecallRecollection))*100,
                          RecallGuess_per    = (RecallGuess/(RecallN - RecallRecollection - RecallFamiliarity))*100,
                          AFCNoMemory        = table(resCon)[1], 
                          AFCRecollection    = table(resCon)[2],
                          AFCFamiliarity     = table(resCon)[3],
                          AFCGuess           = table(resCon)[4],
                          AFCN               = sum(table(resCon)),
                          AFCRec_per         = (AFCRecollection/AFCN)*100,
                          AFCFam_per         = (AFCFamiliarity/AFCN)*100,
                          AFCFam_per_ind     = (AFCFamiliarity/(AFCN - AFCRecollection))*100,
                          AFCGuess_per       = (AFCGuess/(AFCN - AFCRecollection - AFCFamiliarity))*100)