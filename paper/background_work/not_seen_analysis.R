# This script quickly calculates the not seen rates

######################################################
# Path to parent folder schemaVR
path2parent <- "D:/Alex/Laptop/Desktop/schemaVR" # This need to be changed to run this document
######################################################

# Loading all .RData files
load("C:/Users/aq01/Desktop/schemaVR/paper/data/dataSchemaVR1_cleaned.RData")
load("C:/Users/aq01/Desktop/schemaVR/paper/data/dataSchemaVR2_cleaned.RData")
urskload(paste0(path2parent, "/schemaVR3/data/dataSchemaVR3_cleaned.RData"))
load(paste0(path2parent, "/schemaVR4/data/dataSchemaVR4_cleaned.RData"))
# Lib
library(plyr)

# Exp 1
dataSchemaVR1$notSeen_recall <- dataSchemaVR1$recallNoMemory == 1
dataSchemaVR1$notSeen_3AFC   <- dataSchemaVR1$resCon == 1

# Exp 2
dataSchemaVR2$notSeen_recall <- dataSchemaVR2$recallMemory == 0
dataSchemaVR2$notSeen_3AFC   <- dataSchemaVR2$resCon == 0

# Exp 3a
dataSchemaVR3$notSeen_recall <- dataSchemaVR3$recallMemory == 0
dataSchemaVR3$notSeen_3AFC   <- dataSchemaVR3$resCon == 0

# Exp 3b
dataSchemaVR4$notSeen_recall <- dataSchemaVR4$recallMemory == 0
dataSchemaVR4$notSeen_3AFC   <- dataSchemaVR4$resCon == 0

# Aggregate
exp1_notSeen <- ddply(dataSchemaVR1, c('subNum'), summarise, 
                      sum_recall = sum(notSeen_recall, na.rm = TRUE),
                      na_recall  = sum(is.na(notSeen_recall)),
                      sum_3AFC   = sum(notSeen_3AFC))
exp1_notSeen$exp  <- 1

exp2_notSeen <- ddply(dataSchemaVR2, c('subNum'), summarise, 
                      sum_recall = sum(notSeen_recall, na.rm = TRUE),
                      na_recall  = sum(is.na(notSeen_recall)),
                      sum_3AFC   = sum(notSeen_3AFC))
exp2_notSeen$exp  <- 2

exp3a_notSeen <- ddply(dataSchemaVR3, c('subNum'), summarise, 
                      sum_recall = sum(notSeen_recall, na.rm = TRUE),
                      na_recall  = sum(is.na(notSeen_recall)),
                      sum_3AFC   = sum(notSeen_3AFC))
exp3a_notSeen$exp  <- 3

exp3b_notSeen <- ddply(dataSchemaVR4, c('subNum'), summarise, 
                       sum_recall = sum(notSeen_recall, na.rm = TRUE),
                       na_recall  = sum(is.na(notSeen_recall)),
                       sum_3AFC   = sum(notSeen_3AFC))
exp3b_notSeen$exp  <- 3

all_notSeen <- rbind(exp1_notSeen, exp2_notSeen, exp3a_notSeen, exp3b_notSeen)


cor.test(all_notSeen$sum_recall, all_notSeen$sum_3AFC)


all_notSeen$mean <- (all_notSeen$sum_3AFC + all_notSeen$sum_recall)/2

# Aggregate across Exp
ddply(all_notSeen, c('exp'), summarise, exp_mean = mean(mean), exp_sd = sd(mean))


# Are the people with none seen objects really so bad?
all_notSeen$subNum[all_notSeen$sum_recall >= 10]

# 1
mean(dataSchemaVR2$accRecall[dataSchemaVR2$subNum == '6KB3P5'])
mean(dataSchemaVR2$accAFC[dataSchemaVR2$subNum == '6KB3P5'])
# vs. 
mean(dataSchemaVR2$accRecall[dataSchemaVR2$subNum != '6KB3P5'], na.rm = TRUE)
mean(dataSchemaVR2$accAFC[dataSchemaVR2$subNum != '6KB3P5'])

# 2
mean(dataSchemaVR2$accRecall[dataSchemaVR2$subNum == 'PPONCW'])
mean(dataSchemaVR2$accAFC[dataSchemaVR2$subNum == 'PPONCW'])
# vs. 
mean(dataSchemaVR2$accRecall[dataSchemaVR2$subNum != 'PPONCW'], na.rm = TRUE)
mean(dataSchemaVR2$accAFC[dataSchemaVR2$subNum != 'PPONCW'])

accRecall <- ddply(dataSchemaVR2, c('subNum'), summarise, accRecall = mean(accRecall, na.rm = TRUE))


mean(accRecall$accRecall > 0.2)
mean(accRecall$accRecall > 0.1)
