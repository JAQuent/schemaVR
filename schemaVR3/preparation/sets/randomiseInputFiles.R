# This script randomises the retrieval input files for schemaVR3
setwd("U:/Projects/schemaVR/schemaVR3/preparation/sets")
n                 <- 26
setNames          <- as.character(c(111, 246, 388, 498, 848))

# Loop
for(set in setNames){
  tempData <- read.table(paste('schemaVR3_encodingInputFile_VR_', set, '.txt', sep = ''), 
                         header = FALSE)
  for(i in 1:n){
    write.table(tempData[sample(20),],
                paste('schemaVR3_retrievalInputFile_VR_', set, '_', i, '.txt', sep = ''),
                row.names = FALSE,
                col.names = FALSE,
                quote     = FALSE,
                sep = '\t')
  }
}