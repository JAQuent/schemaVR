n <- 50
allNames  <- c('microwave', 
               'kitchenRoll', 
               'saucepan', 
               'toaster', 
               'fruitBowl', 
               'teaPot', 
               'knife', 
               'mixer', 
               'bread', 
               'dishes', 
               'glassContainer', 
               'mug', 
               'towel', 
               'toy', 
               'bookPile', 
               'umbrella', 
               'hat', 
               'helmet', 
               'calendar',
               'fan')

xRotations <- c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
yRotations <- c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
zRotations <- c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)

data <- data.frame(names = allNames, xRotations = xRotations, yRotations = yRotations, zRotations = zRotations)

for(subject in 1:n){
  write.table(data[sample(1:length(xRotations)), ],  
              file = paste('output/inputFile_', as.character(subject), '.txt', sep = ''), 
              sep = "\t",
              row.names = F,
              col.names = F, 
              quote = F)
}
