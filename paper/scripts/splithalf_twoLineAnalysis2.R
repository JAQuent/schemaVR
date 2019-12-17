# Script performance a split half two line analysis for recall: Part 2
# Version 1.0
# Date: 13/11/2019
# Author: Joern Alexander Quent
# /*
# ----------------------------- Libraries, settings and functions ---------------------------
# */
# Remove all
rm(list = ls(all.names = TRUE))

# Setting seed and cores
set.seed(144)

# Libraries
library(brms)
library(rslurm)
library(ggplot2)
library(MRColour)
library(plyr)

# Setting wd
setwd("U:/Projects/schemaVR/schemaVR_paper/scripts")

# Load auxilliary data
load("splithalf_recall_prep.RData")

# /*
# ----------------------------- Loading data ---------------------------
# */
batch0 <- readRDS('_rslurm_splithalf_recall1/results_0.RDS', refhook = NULL)
batch1 <- readRDS('_rslurm_splithalf_recall1/results_1.RDS', refhook = NULL)


# /*
# ----------------------------- Combining data ---------------------------
# */
nSims <- (nBr*nRuns)/2
# Batch 0
for(i in 1:nSims){
  if(!exists(paste('m_br', which(br_recall == batch0[[i]]$br), sep = ''))){
    assign(paste('m_br', which(br_recall == batch0[[i]]$br), sep = ''), batch0[[i]]$model)
  } else {
    assign(paste('m_br', which(br_recall == batch0[[i]]$br), sep = ''), 
           combine_models(get(paste('m_br', which(br_recall == batch0[[i]]$br), sep = '')),
                          batch0[[i]]$model))
  } 
}

# Batch 1
for(i in 1:nSims){
  if(!exists(paste('m_br', which(br_recall == batch1[[i]]$br), sep = ''))){
    assign(paste('m_br', which(br_recall == batch1[[i]]$br), sep = ''), batch1[[i]]$model)
  } else {
    assign(paste('m_br', which(br_recall == batch1[[i]]$br), sep = ''), 
           combine_models(get(paste('m_br', which(br_recall == batch1[[i]]$br), sep = '')),
                          batch1[[i]]$model))
  }
}

rm(batch0)
rm(batch1)

# /*
# ----------------------------- Extracting fixef and CI ---------------------------
# */
fixefs <- list()
for(i in 1:nBr){
  fixefs[[i]] <- fixef(get(paste('m_br', i, sep = '')))
}


# Extracting CI
for(i in 1:nBr){
  if(fixefs[[i]][2,4] < 0 & fixefs[[i]][3,3] > 0){
    uShape <- 1
  } else {
    uShape <- 0
  }
  if(i == 1){
    recall_CI <- data.frame(br = c(br_recall[i], br_recall[i], br_recall[i], br_recall[i]),
                            Line = c('low', 'low', 'high', 'high'),
                            Bound = c('Q2.5', 'Q97.5', 'Q2.5', 'Q97.5'),
                            Beta = c(fixefs[[i]][2,3], fixefs[[i]][2,4], fixefs[[i]][3,3], fixefs[[i]][3,4]),
                            uShape = rep(uShape, 4))
  } else {
    recall_CI <- rbind(recall_CI, data.frame(br = c(br_recall[i], br_recall[i], br_recall[i], br_recall[i]),
                                             Line = c('low', 'low', 'high', 'high'),
                                             Bound = c('Q2.5', 'Q97.5', 'Q2.5', 'Q97.5'),
                                             Beta = c(fixefs[[i]][2,3], fixefs[[i]][2,4], fixefs[[i]][3,3], fixefs[[i]][3,4]),
                                             uShape = rep(uShape, 4)))
  }
}

# Add line ID
recall_CI$lineId <- rep(1:(nBr*2), each = 2)

# /*
# ----------------------------- Plot results ---------------------------
# */
# Which BR show u-shape and get segments for arrows
uShape_df    <- ddply(recall_CI, c('br'), summarise, uShape = uShape[1])
nSegments    <- sum(uShape_df$uShape == 1) 
lineSegments <- data.frame(x = rep(-8, nSegments),
                           y = uShape_df[uShape_df$uShape == 1, 1],
                           xend = rep(-6, nSegments),
                           yend = uShape_df[uShape_df$uShape == 1, 1])

# Create plot
ggplot(recall_CI, aes(x = Beta, y = br, group = lineId, colour = Line)) + 
  geom_point() + 
  geom_line(aes(group = lineId)) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  labs(title = '95% CI for two slopes (Recall accuracy)', y = 'Breaking point') +
  scale_color_mrc()