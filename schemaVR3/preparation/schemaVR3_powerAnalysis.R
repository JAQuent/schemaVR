####################################
# Libraries
library(lmerTest)
library(simr)
# Green, Peter, and Catriona J. Macleod. 2016. “SIMR: An R package for power analysis of generalized linear mixed models by simulation.” Methods in Ecology and Evolution 7 (4): 493–98. doi:10.1111/2041-210X.12504.

####################################
# Loading and preparing data
load("U:/Projects/schemaVR/report_firstYear/report/data/exp2Data.RData")
dataSchemaVR2 <- combData
rm(combData)

#Add (non-)kitchen object factor to data
dataSchemaVR2$expectedInKitchen                                      <- 'non-kitchen'
dataSchemaVR2[which(dataSchemaVR2$objNum < 13), 'expectedInKitchen'] <- 'kitchen'

# Notes and decision on participants:
# Participant #20 to #24: Exclude participants because they did the  wrong objLocTargetRating
dataSchemaVR2 <- subset(dataSchemaVR2, subNum >= 25)
# Participant #20 to #27: Foil2 for umbrella was not saved.
# Participant #25 to #27: Rated object 16 at location 1 instead of 14. This value is therefore missing.
# Participant #22: Seen objects twice but is excluded anyway. 
# Participant #26: Check recall microwave because it was correct (it is). The 2nd rating was 100 not 0.
dataSchemaVR2[which(dataSchemaVR2$subNum == 26 & dataSchemaVR2$objNum == 3), 'generalRatingPost'] <- 100

# Response given for judgement (0 = no memory, 1 = remember, 2 = familiar, 3 = guess)
dataSchemaVR2$recallMemory[which(dataSchemaVR2$recallMemory == -1)] <- NA # Code missing value

# Recoding 3AFc recollection
recollection3AFC               <- rep(0, length(dataSchemaVR2$resCon))
recollection3AFC[dataSchemaVR2$resCon == 1] <- 1
recollection3AFC[is.na(dataSchemaVR2$resCon)] <- NA
recollection3AFC               <- as.factor(recollection3AFC)
levels(recollection3AFC)       <- c('not recollected', 'recollected')
dataSchemaVR2$recollection3AFC <- recollection3AFC

####################################
# Model that I base my power analysis on schemaVR2_rf_model8
originalModel <- glmer(recollection3AFC ~  scale(objLocTargetRating)  +
                               (1 | subNum) +
                               (1 | objNum), 
                             data = subset(dataSchemaVR2, dataSchemaVR2$expectedInKitchen == 'kitchen'),
                             family = binomial,
                             control = glmerControl(optimizer = "bobyqa"),
                             nAGQ = 1)

####################################
# Power analysis
# Retrospective ‘observed power’ 
retroPower <- powerSim(originalModel,
                       test = fixed("scale(objLocTargetRating)", method = 'z'),
                       progress = TRUE)

# Calculating the power curve 
# Number of runs in monte-carlo simulation
simrOptions(nsim = 1000)

# Extent model to 25 participants to plot power curve
extendedModel <- extend(originalModel,
                              along = 'subNum',
                              n = 10000)

pc1 <- powerCurve(extendedModel, 
                  test = fixed("scale(objLocTargetRating)", method = 'z'),
                  along = "subNum", 
                  progress = TRUE)

save(retroPower,
     pc1, 
     originalModel, 
     extendedModel, 
     file = paste("schemaVR3_poweAnalysisData_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".RData", sep = ""))