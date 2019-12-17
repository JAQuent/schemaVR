# Script performance a split half two line analysis for AFC: Part 2
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
load("splithalf_AFC_prep.RData")

# /*
# ----------------------------- Loading data ---------------------------
# */
batch0 <- readRDS('_rslurm_splithalf_AFC1/results_0.RDS', refhook = NULL)
batch1 <- readRDS('_rslurm_splithalf_AFC1/results_1.RDS', refhook = NULL)


# /*
# ----------------------------- Combining data ---------------------------
# */
nSims <- (nBr*nRuns)/2
# Batch 0
for(i in 1:nSims){
  if(!exists(paste('m_br', which(br_AFC == batch0[[i]]$br), sep = ''))){
    assign(paste('m_br', which(br_AFC == batch0[[i]]$br), sep = ''), batch0[[i]]$model)
  } else {
    assign(paste('m_br', which(br_AFC == batch0[[i]]$br), sep = ''), 
           combine_models(get(paste('m_br', which(br_AFC == batch0[[i]]$br), sep = '')),
                          batch0[[i]]$model))
  } 
}

# Batch 1
for(i in 1:nSims){
  if(!exists(paste('m_br', which(br_AFC == batch1[[i]]$br), sep = ''))){
    assign(paste('m_br', which(br_AFC == batch1[[i]]$br), sep = ''), batch1[[i]]$model)
  } else {
    assign(paste('m_br', which(br_AFC == batch1[[i]]$br), sep = ''), 
           combine_models(get(paste('m_br', which(br_AFC == batch1[[i]]$br), sep = '')),
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
    AFC_CI <- data.frame(br = c(br_AFC[i], br_AFC[i], br_AFC[i], br_AFC[i]),
                            Line = c('low', 'low', 'high', 'high'),
                            Bound = c('Q2.5', 'Q97.5', 'Q2.5', 'Q97.5'),
                            Beta = c(fixefs[[i]][2,3], fixefs[[i]][2,4], fixefs[[i]][3,3], fixefs[[i]][3,4]),
                            uShape = rep(uShape, 4))
  } else {
    AFC_CI <- rbind(AFC_CI, data.frame(br = c(br_AFC[i], br_AFC[i], br_AFC[i], br_AFC[i]),
                                             Line = c('low', 'low', 'high', 'high'),
                                             Bound = c('Q2.5', 'Q97.5', 'Q2.5', 'Q97.5'),
                                             Beta = c(fixefs[[i]][2,3], fixefs[[i]][2,4], fixefs[[i]][3,3], fixefs[[i]][3,4]),
                                             uShape = rep(uShape, 4)))
  }
}

# Add line ID
AFC_CI$lineId <- rep(1:(nBr*2), each = 2)

# /*
# ----------------------------- Plot results ---------------------------
# */
# Which BR show u-shape and get segments for arrows
uShape_df    <- ddply(AFC_CI, c('br'), summarise, uShape = uShape[1])
nSegments    <- sum(uShape_df$uShape == 1) 
lineSegments <- data.frame(x = rep(-8, nSegments),
                           y = uShape_df[uShape_df$uShape == 1, 1],
                           xend = rep(-6, nSegments),
                           yend = uShape_df[uShape_df$uShape == 1, 1])

# Create plot
ggplot(AFC_CI, aes(x = Beta, y = br, colour = Line)) + 
  geom_point() + 
  geom_line(aes(group = lineId)) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  labs(title = '95% CI for two slopes (AFC accuracy)', y = 'Breaking point') +
  geom_segment(mapping = aes(x = x, y = y, xend = xend, yend = yend),
               data = lineSegments,
               arrow = arrow(length = unit(0.5, "cm")),
               colour = 'red') +
  scale_color_mrc() + 
  theme(legend.justification = c(1, 0), legend.position = c(1, 0))
# Plug legend into right bottom corner