####################################
# This script extracts the norm expectancy for the 5 sets
# So that Rik can use it to do the power analysis

####################################
# For schemaVR3 sets
# Preparation
setwd("U:/Projects/schemaVR/schemaVR3")
normativeData  <- read.table('data/normativeData.txt', header = TRUE, sep = "\t")
sets           <- read.csv("preparation/schemaVR3_sets.csv")

# Extracting information
ratingsForSets <- data.frame(objNam   = rep(sets$Objects, 5),
                             objNum   = rep(1:20, 5),
                             set      = rep(1:5, each = 20),
                             location = c(sets$Set1, sets$Set2, sets$Set3, sets$Set4, sets$Set5))

ratingsForSets$expectancy <- normativeData[cbind(seq_along(1:20),  ratingsForSets$location + 2)]

# Write to file
write.csv(ratingsForSets, "preparation/ratingsForSets.csv", row.names = FALSE)

####################################
# For schemaVR2 set
# Preparation
schemaVR2_locations <- c(4, 10, 16, 15, 6, 11, 9, 13, 5, 8, 1, 14, 20, 18, 12, 19, 7, 17, 2, 3)
schemaVR2_ratings   <- data.frame(objNam = sets$Objects,
                                  objNum = 1:20,
                                  location = schemaVR2_locations,
                                  expectancy =  normativeData[cbind(seq_along(1:20),  schemaVR2_locations + 2)])
write.csv(schemaVR2_ratings, 'preparation/schemaVR2_ratings.csv', row.names = FALSE)
