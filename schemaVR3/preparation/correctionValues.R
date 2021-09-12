# This script is here to calculate the correctionValues for #16 
# because they were accidentally overwritten.
library(data.table)
fileNam2 <- 'U:/Projects/schemaVR/schemaVR3/data/correctionValues_'
subNum   <- c(14:15, 17) # based on the subjects on same the session
correVal <- as.matrix(rbindlist( lapply(paste(fileNam2, subNum,'.txt', sep = ''), fread)))

write.table(data.frame(x = mean(correVal[,1]), z = mean(correVal[,2])),
            file = paste(fileNam2, 16,'.txt', sep = ''),
            row.names = FALSE,
            col.names = FALSE,
            quote     = FALSE,
            sep       = ' ')