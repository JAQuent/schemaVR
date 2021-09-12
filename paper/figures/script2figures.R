# Script creates figures for the schemaVR paper
# Version 2.0
# Date: 12/05/2021
# Author: Joern Alexander Quent

# To do cap the plots correctly 


# /* 
# ----------------------------- Libraries, settings and functions ---------------------------
# */
######################################################
# Path to parent folder schemaVR
path2parent <- "C:/Users/aq01/Desktop/schemaVR" # This need to be changed to run this document
######################################################
# Setting wd
setwd(paste0(path2parent, "/paper/figures"))

# Libraries
library(cowplot)
library(ggplot2)
library(MRColour)
library(assortedRFunctions)
library(brms)
library(plyr)
library(latex2exp)
library(polspline)
library(ggsignif)


# Parameters across plots
drawSamples  <- 1000
lineAlpha    <- 0.01 # Quad plots
lineSize     <- 0.1
se_colour    <- 'red'
loess_colour <- 'red'
steps        <- 20 # steps for kitchen objects raster

# Set theme default
updatedTheme <- theme_grey() + theme(text = element_text(size = 10),
                                     plot.title = element_text(size = 10),
                                     legend.title = element_text(size=6), 
                                     legend.text = element_text(size=6))
theme_set(updatedTheme)
# see also here https://rpubs.com/Koundy/71792
# see also here http://sape.inf.usi.ch/quick-reference/ggplot2/themes
# see also http://www.sthda.com/english/articles/24-ggpubr-publication-ready-plots/

# /* 
# ----------------------------- Load all models ---------------------------
# */

# Load data for recall
load(paste0(path2parent, "/schemaVR4/analysis/schemaVR3_4_recall_sequential_20210516_193155.RData"))
sd_value_recall  <- sd_value

# Load data for 3AFC
load(paste0(path2parent, "/schemaVR4/analysis/schemaVR3_4_AFC_sequential_20210520_130825.RData"))
sd_value_AFC     <- sd_value

# Load data for RK
load(paste0(path2parent, "/schemaVR4/analysis/schemaVR3_4_RK_sequential_20210515_141354.RData"))
sd_value_RK      <- sd_value

# /* 
# ----------------------------- Figure 2 ---------------------------
# */
# /* 
# ----------------------------- schemaVR1 ---------------------------
# */
######## Recall accuracy
# Plot by using brms function
recallPlot1 <- plot(marginal_effects(model_schemaVR1_recall2, 
                                     spaghetti = TRUE, 
                                     nsamples = drawSamples), points = FALSE)

# Extract plot object, add title etc. and change parameters
recallPlot1 <- recallPlot1$sExp
recallPlot1 <- recallPlot1 + 
  labs(y = 'p(recall accuracy)',
       x = "Expectancy")
recallPlot1$layers[[1]]$aes_params$colour <- rgb(0, 0, 0, lineAlpha)

# Add Loess smoothing based on the data
recallPlot1 <- recallPlot1 + geom_smooth(data = model_schemaVR1_recall2$data, 
                                         mapping = aes(x = sExp , y = accRecall), 
                                         colour = loess_colour, 
                                         alpha = 0.1, 
                                         method = 'loess', 
                                         fill = se_colour)

# Displaying from 0 to 1 on y axis
recallPlot1 <- recallPlot1 + coord_cartesian(ylim = c(0, 1), expand = FALSE)

# Reorder of layers so that the loess line is in the background
temp1 <- recallPlot1$layers[[1]]
temp2 <- recallPlot1$layers[[2]]
recallPlot1$layers[[1]] <- recallPlot1$layers[[3]]
recallPlot1$layers[[2]] <- temp1
recallPlot1$layers[[3]] <- temp2


########  AFC
# Plot by using brms function
AFCPlot1 <- plot(marginal_effects(model_schemaVR1_AFC2, 
                                     spaghetti = TRUE, 
                                     nsamples = drawSamples), points = FALSE)

# Extract plot object, add title etc. and change parameters
AFCPlot1 <- AFCPlot1$sExp
AFCPlot1 <- AFCPlot1 + 
  labs(y = 'p(3AFC accuracy)',
       x = "Expectancy")
AFCPlot1$layers[[1]]$aes_params$colour <- rgb(0, 0, 0, lineAlpha)

# Add Loess smoothing based on the data
AFCPlot1 <- AFCPlot1 + geom_smooth(data = model_schemaVR1_AFC2$data, 
                                   mapping = aes(x = sExp , y = accAFC), 
                                   colour = loess_colour, 
                                   alpha = 0.1, 
                                   method = 'loess', 
                                   fill = se_colour)

# Displaying from 0 to 1 on y axis
AFCPlot1 <- AFCPlot1 + coord_cartesian(ylim = c(0, 1), expand = FALSE)

# Reorder of layers so that the loess line is in the background
temp1 <- AFCPlot1$layers[[1]]
temp2 <- AFCPlot1$layers[[2]]
AFCPlot1$layers[[1]] <- AFCPlot1$layers[[3]]
AFCPlot1$layers[[2]] <- temp1
AFCPlot1$layers[[3]] <- temp2


# /* 
# ----------------------------- schemaVR2 ---------------------------
# */
######## Recall accuracy
# Plot by using brms function
recallPlot2 <- plot(marginal_effects(model_schemaVR2_recall, 
                                     spaghetti = TRUE, 
                                     nsamples = drawSamples), points = FALSE)

# Extract plot object, add title etc. and change parameters
recallPlot2 <- recallPlot2$sExp
recallPlot2 <- recallPlot2 + 
  labs(y = 'p(recall accuracy)',
       x = "Expectancy")
recallPlot2$layers[[1]]$aes_params$colour <- rgb(0, 0, 0, lineAlpha)

# Add Loess smoothing based on the data
recallPlot2 <- recallPlot2 + geom_smooth(data = model_schemaVR2_recall$data, 
                                         mapping = aes(x = sExp , y = accRecall), 
                                         colour = loess_colour, 
                                         alpha = 0.1, 
                                         method = 'loess', 
                                         fill = se_colour)

# Displaying from 0 to 1 on y axis
recallPlot2 <- recallPlot2 + coord_cartesian(ylim = c(0, 1), expand = FALSE)

# Reorder of layers so that the loess line is in the background
temp1 <- recallPlot2$layers[[1]]
temp2 <- recallPlot2$layers[[2]]
recallPlot2$layers[[1]] <- recallPlot2$layers[[3]]
recallPlot2$layers[[2]] <- temp1
recallPlot2$layers[[3]] <- temp2



########  AFC
# Plot by using brms function
AFCPlot2 <- plot(marginal_effects(model_schemaVR2_AFC, 
                                  spaghetti = TRUE, 
                                  nsamples = drawSamples), points = FALSE)

# Extract plot object, add title etc. and change parameters
AFCPlot2 <- AFCPlot2$sExp
AFCPlot2 <- AFCPlot2 + 
  labs(y = 'p(3AFC accuracy)',
       x = "Expectancy")
AFCPlot2$layers[[1]]$aes_params$colour <- rgb(0, 0, 0, lineAlpha)

# Add Loess smoothing based on the data
AFCPlot2 <- AFCPlot2 + geom_smooth(data = model_schemaVR2_AFC$data, 
                                   mapping = aes(x = sExp , y = accAFC), 
                                   colour = loess_colour, 
                                   alpha = 0.1, 
                                   method = 'loess', 
                                   fill = se_colour)

# Displaying from 0 to 1 on y axis
AFCPlot2 <- AFCPlot2 + coord_cartesian(ylim = c(0, 1), expand = FALSE)

# Reorder of layers so that the loess line is in the background
temp1 <- AFCPlot2$layers[[1]]
temp2 <- AFCPlot2$layers[[2]]
AFCPlot2$layers[[1]] <- AFCPlot2$layers[[3]]
AFCPlot2$layers[[2]] <- temp1
AFCPlot2$layers[[3]] <- temp2


# /* 
# ----------------------------- schemaVR3_4 ---------------------------
# */
######## Recall accuracy
# Plot by using brms function
recallPlot3 <- plot(marginal_effects(model_schemaVR3_4_recall, 
                                     spaghetti = TRUE, 
                                     nsamples = drawSamples), points = FALSE)

# Extract plot object, add title etc. and change parameters
recallPlot3 <- recallPlot3$sExp
recallPlot3 <- recallPlot3 + 
  labs(y = 'p(recall accuracy)',
       x = "Expectancy")
recallPlot3$layers[[1]]$aes_params$colour <- rgb(0, 0, 0, lineAlpha)

# Add Loess smoothing based on the data
recallPlot3 <- recallPlot3 + geom_smooth(data = model_schemaVR3_4_recall$data, 
                                         mapping = aes(x = sExp , y = accRecall), 
                                         colour = loess_colour, 
                                         alpha = 0.1, 
                                         method = 'loess', 
                                         fill = se_colour)

# Displaying from 0 to 1 on y axis
recallPlot3 <- recallPlot3 + coord_cartesian(ylim = c(0, 1), expand = FALSE)

# Reorder of layers so that the loess line is in the background
temp1 <- recallPlot3$layers[[1]]
temp2 <- recallPlot3$layers[[2]]
recallPlot3$layers[[1]] <- recallPlot3$layers[[3]]
recallPlot3$layers[[2]] <- temp1
recallPlot3$layers[[3]] <- temp2



########  AFC
# Plot by using brms function
AFCPlot3 <- plot(marginal_effects(model_schemaVR3_4_AFC, 
                                  spaghetti = TRUE, 
                                  nsamples = drawSamples), points = FALSE)

# Extract plot object, add title etc. and change parameters
AFCPlot3 <- AFCPlot3$sExp
AFCPlot3 <- AFCPlot3 + 
  labs(y = 'p(3AFC accuracy)',
       x = "Expectancy")
AFCPlot3$layers[[1]]$aes_params$colour <- rgb(0, 0, 0, lineAlpha)

# Add Loess smoothing based on the data
AFCPlot3 <- AFCPlot3 + geom_smooth(data = model_schemaVR3_4_AFC$data, 
                                   mapping = aes(x = sExp , y = accAFC), 
                                   colour = loess_colour, 
                                   alpha = 0.1, 
                                   method = 'loess', 
                                   fill = se_colour)

# Displaying from 0 to 1 on y axis
AFCPlot3 <- AFCPlot3 + coord_cartesian(ylim = c(0, 1), expand = FALSE)

# Reorder of layers so that the loess line is in the background
temp1 <- AFCPlot3$layers[[1]]
temp2 <- AFCPlot3$layers[[2]]
AFCPlot3$layers[[1]] <- AFCPlot3$layers[[3]]
AFCPlot3$layers[[2]] <- temp1
AFCPlot3$layers[[3]] <- temp2



# /* 
# ----------------------------- Combine into 1 figure ---------------------------
# */
# Make 1 figure
figure2 <- plot_grid(recallPlot1, AFCPlot1, 
                     recallPlot2, AFCPlot2, 
                     recallPlot3, AFCPlot3, 
                     ncol = 2, labels = 'AUTO')

# save as image
save_plot("figure2.png", figure2,
          base_height = 16/cm(1),
          base_width = 12/cm(1),
          base_aspect_ratio = 1)


# /* 
# ----------------------------- Figure 3 ---------------------------
# */
# 2 x 2 for remember & familiarity

# /* 
# ----------------------------- schemaVR2 ---------------------------
# */
######## Remember
# Plot by using brms function
rememPlot1 <- plot(marginal_effects(model_schemaVR2_rem, 
                                    spaghetti = TRUE, 
                                    nsamples = drawSamples), points = FALSE)

# Extract plot object, add title etc. and change parameters
rememPlot1 <- rememPlot1$sExp
rememPlot1 <- rememPlot1 + 
  labs(y = 'p(Recollection)',
       x = "Expectancy")
rememPlot1$layers[[1]]$aes_params$colour <- rgb(0, 0, 0, lineAlpha)

# Add Loess smoothing based on the data
rememPlot1 <- rememPlot1 + geom_smooth(data = model_schemaVR2_rem$data, 
                                       mapping = aes(x = sExp , y = remembered), 
                                       colour = loess_colour, 
                                       alpha = 0.1, 
                                       method = 'loess', 
                                       fill = se_colour)

# Displaying from 0 to 1 on y axis
rememPlot1 <- rememPlot1 + coord_cartesian(ylim = c(0, 1), expand = FALSE)

# Reorder of layers so that the loess line is in the background
temp1 <- rememPlot1$layers[[1]]
temp2 <- rememPlot1$layers[[2]]
rememPlot1$layers[[1]] <- rememPlot1$layers[[3]]
rememPlot1$layers[[2]] <- temp1
rememPlot1$layers[[3]] <- temp2

# /* 
# ----------------------------- schemaVR3 ---------------------------
# */
######## Remember
# Plot by using brms function
rememPlot2 <- plot(marginal_effects(model_schemaVR4_rem, 
                                    spaghetti = TRUE, 
                                    nsamples = drawSamples), points = FALSE)

# Extract plot object, add title etc. and change parameters
rememPlot2 <- rememPlot2$sExp
rememPlot2 <- rememPlot2 + 
  labs(y = 'p(Recollection)',
       x = "Expectancy")
rememPlot2$layers[[1]]$aes_params$colour <- rgb(0, 0, 0, lineAlpha)

# Add Loess smoothing based on the data
rememPlot2 <- rememPlot2 + geom_smooth(data = model_schemaVR4_rem$data, 
                                       mapping = aes(x = sExp , y = remembered), 
                                       colour = loess_colour, 
                                       alpha = 0.1, 
                                       method = 'loess', 
                                       fill = se_colour)

# Displaying from 0 to 1 on y axis
rememPlot2 <- rememPlot2 + coord_cartesian(ylim = c(0, 1), expand = FALSE)

# Reorder of layers so that the loess line is in the background
temp1 <- rememPlot2$layers[[1]]
temp2 <- rememPlot2$layers[[2]]
rememPlot2$layers[[1]] <- rememPlot2$layers[[3]]
rememPlot2$layers[[2]] <- temp1
rememPlot2$layers[[3]] <- temp2

######## Familiar
# Plot by using brms function
familiarPlot1 <- plot(marginal_effects(model_schemaVR2_familiar_ind, 
                                       spaghetti = TRUE, 
                                       nsamples = drawSamples), points = FALSE)

# Extract plot object, add title etc. and change parameters
familiarPlot1 <- familiarPlot1$sExp
familiarPlot1 <- familiarPlot1 + 
  labs(y = 'p(Familiarity)',
       x = "Expectancy")
familiarPlot1$layers[[1]]$aes_params$colour <- rgb(0, 0, 0, lineAlpha)

# Add Loess smoothing based on the data
familiarPlot1 <- familiarPlot1 + geom_smooth(data = model_schemaVR2_familiar_ind$data, 
                                             mapping = aes(x = sExp , y = familiar_ind), 
                                             colour = loess_colour, 
                                             alpha = 0.1, 
                                             method = 'loess', 
                                             fill = se_colour)

# Displaying from 0 to 1 on y axis
familiarPlot1 <- familiarPlot1 + coord_cartesian(ylim = c(0, 1), expand = FALSE)

# Reorder of layers so that the loess line is in the background
temp1 <- familiarPlot1$layers[[1]]
temp2 <- familiarPlot1$layers[[2]]
familiarPlot1$layers[[1]] <- familiarPlot1$layers[[3]]
familiarPlot1$layers[[2]] <- temp1
familiarPlot1$layers[[3]] <- temp2

# /* 
# ----------------------------- schemaVR3 ---------------------------
# */
######## Remember
# Plot by using brms function
familiarPlot2 <- plot(marginal_effects(model_schemaVR4_familiar_ind, 
                                       spaghetti = TRUE, 
                                       nsamples = drawSamples), points = FALSE)

# Extract plot object, add title etc. and change parameters
familiarPlot2 <- familiarPlot2$sExp
familiarPlot2 <- familiarPlot2 + 
  labs(y = 'p(Familiarity)',
       x = "Expectancy")
familiarPlot2$layers[[1]]$aes_params$colour <- rgb(0, 0, 0, lineAlpha)

# Add Loess smoothing based on the data
familiarPlot2 <- familiarPlot2 + geom_smooth(data = model_schemaVR4_familiar_ind$data, 
                                             mapping = aes(x = sExp, y = familiar_ind), 
                                             colour = loess_colour, 
                                             alpha = 0.1, 
                                             method = 'loess', 
                                             fill = se_colour)

# Displaying from 0 to 1 on y axis
familiarPlot2 <- familiarPlot2 + coord_cartesian(ylim = c(0, 1), expand = FALSE)

# Reorder of layers so that the loess line is in the background
temp1 <- familiarPlot2$layers[[1]]
temp2 <- familiarPlot2$layers[[2]]
familiarPlot2$layers[[1]] <- familiarPlot2$layers[[3]]
familiarPlot2$layers[[2]] <- temp1
familiarPlot2$layers[[3]] <- temp2


# /* 
# ----------------------------- Combine into 1 figure ---------------------------
# */
# Make 1 figure
figure3 <- plot_grid(rememPlot1, familiarPlot1, rememPlot2, familiarPlot2, 
                     ncol = 2, labels = 'AUTO')

# save as image
save_plot("figure3.png", figure3,
          base_height = 12/cm(1),
          base_width =  12/cm(1),
          base_aspect_ratio = 1)


# /* 
# ----------------------------- Supplement: Spread across participants ---------------------------
# */
# Loading data
# Loading all .RData files
load("C:/Users/aq01/Desktop/schemaVR/paper/data/dataSchemaVR1_cleaned.RData")
load("C:/Users/aq01/Desktop/schemaVR/paper/data/dataSchemaVR2_cleaned.RData")
load("C:/Users/aq01/Desktop/schemaVR/paper/data/dataSchemaVR3_cleaned.RData")
load("C:/Users/aq01/Desktop/schemaVR/schemaVR4/data/dataSchemaVR4_cleaned.RData")

# Combining data sets 
combinedRatings <- data.frame(Participant = c(as.character(dataSchemaVR1$subNum),
                                              as.character(dataSchemaVR2$subNum),
                                              as.character(dataSchemaVR3$subNum),
                                              as.character(dataSchemaVR4$subNum)),
                              objNum     = c(dataSchemaVR1$objNum,
                                             dataSchemaVR2$objNum,
                                             dataSchemaVR3$objNum,
                                             dataSchemaVR4$objNum),
                              objNam     = c(dataSchemaVR1$objNam,
                                             dataSchemaVR2$objNam,
                                             dataSchemaVR3$objNam,
                                             dataSchemaVR4$objNam),
                              Expectancy = c(dataSchemaVR1$objLocTargetRating,
                                             dataSchemaVR2$objLocTargetRating,
                                             dataSchemaVR3$objLocTargetRating,
                                             dataSchemaVR4$objLocTargetRating),
                              Experiment = c(rep('1', length(dataSchemaVR1$subNum)),
                                             rep('2', length(dataSchemaVR2$subNum)),
                                             rep('3', length(dataSchemaVR3$subNum)),
                                             rep('3', length(dataSchemaVR4$subNum))))
combinedRatings$Kitchen <- ifelse(combinedRatings$objNum <= 12, 'Kitchen', 'Non-kitchen')

# mrc_pal("secondary")(3) "#822F5A" "#E7A618" "#B5D334"

ratingsSpread <- ggplot(combinedRatings, aes(x = Expectancy, y = Participant)) +
  facet_grid(Experiment ~., space = "free_y", scales = 'free_y') +
  #geom_line(alpha = 0.5) +  
  geom_text(aes(label= objNum, colour = Kitchen), size = 2) +
  #geom_point(aes(colour = Kitchen)) +  
  scale_color_manual(values = c('#E7A618', '#822F5A')) +
  #scale_color_mrc(palette = 'secondary') +
  #scale_y_discrete(limits = combinedRatings$Participant, expand = expand_scale(0.01)) +
  labs(title = 'Spread in expectancy ratings for each participant') +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank())

save_plot("ratingsPlots1.png", ratingsSpread,
          base_height = 15/cm(1),
          base_width = 14/cm(1),
          base_aspect_ratio = 1)

# /* 
# ----------------------------- Recall bias ---------------------------
# */
# Calculate for schemaVR1
# Finding the expectancy of the closest location for an object
dataSchemaVR1$closestObjLocNorm <- NA
# Add normative location rating
for(i in 1:dim(dataSchemaVR1)[1]){
  if(!is.na(dataSchemaVR1$closestLoc[i])){
    # Necessary to use if statement because some values are NA and cannot be used to index
    dataSchemaVR1$closestObjLocNorm[i] <- dataSchemaVR1[i, paste("loc", dataSchemaVR1$closestLoc[i], sep = "")] 
  }
}

schemaVR1_recallBias_data <- ddply(subset(dataSchemaVR1, recallNoMemory == 0 & !is.na(dataSchemaVR1$accRecall)),
                                   c('subNum', 'accRecall'),
                                   summarise, 
                                   closestObjLocNorm = mean(closestObjLocNorm, na.rm = TRUE))

schemaVR1_recallBias     <- ddply(schemaVR1_recallBias_data, 
                                  c('accRecall'),
                                  summarise, 
                                  mean = mean(closestObjLocNorm, na.rm = TRUE),
                                  sd = sd(closestObjLocNorm, na.rm = TRUE))

# Calculate for schemaVR2
dataSchemaVR2$closestObjLocNorm <- NA
# Add normative location rating
for(i in 1:dim(dataSchemaVR2)[1]){
  if(!is.na(dataSchemaVR2$closestLoc[i])){
    # Necessary to use if statement because some values are NA and cannot be used to index
    dataSchemaVR2$closestObjLocNorm[i] <- dataSchemaVR2[i, paste("loc", dataSchemaVR2$closestLoc[i], sep = "")] 
  }
}

schemaVR2_recallBias_data <- ddply(subset(dataSchemaVR2, recallMemory != 0),
                                   c('subNum', 'accRecall'),
                                   summarise, 
                                   closestObjLocNorm = mean(closestObjLocNorm, na.rm = TRUE))

schemaVR2_recallBias     <- ddply(schemaVR2_recallBias_data, 
                                  c('accRecall'),
                                  summarise, 
                                  mean = mean(closestObjLocNorm, na.rm = TRUE),
                                  sd = sd(closestObjLocNorm, na.rm = TRUE))

# Calculate for schemaVR3
load('U:/Projects/schemaVR/schemaVR3/data/schemaVR3_closestLocation.RData')
schemaVR3_recallBias_data <- ddply(subset(dataSchemaVR3, recallMemory != 0),
                                   c('subNum', 'accRecall'),
                                   summarise, 
                                   closestObjLocNorm = mean(closestObjLocNorm, na.rm = TRUE))

schemaVR3_recallBias     <- ddply(schemaVR3_recallBias_data, 
                                  c('accRecall'),
                                  summarise, 
                                  mean = mean(closestObjLocNorm, na.rm = TRUE),
                                  sd = sd(closestObjLocNorm, na.rm = TRUE))

# COmbine data to one data.frame
combined_recallBias_data <- data.frame(Experiment = c(rep('Exp 1', dim(schemaVR1_recallBias_data)[1]),
                                                      rep('Exp 2', dim(schemaVR2_recallBias_data)[1]),
                                                      rep('Exp 3', dim(schemaVR3_recallBias_data)[1])),
                                       subNum = c(as.character(schemaVR1_recallBias_data$subNum),
                                                  as.character(schemaVR2_recallBias_data$subNum),
                                                  as.character(schemaVR3_recallBias_data$subNum)),
                                       Accuracy = c(schemaVR1_recallBias_data$accRecall,
                                                    schemaVR2_recallBias_data$accRecall,
                                                    schemaVR3_recallBias_data$accRecall),
                                       Expectancy = c(schemaVR1_recallBias_data$closestObjLocNorm,
                                                      schemaVR2_recallBias_data$closestObjLocNorm,
                                                      schemaVR3_recallBias_data$closestObjLocNorm))
combined_recallBias_data$Accuracy <- factor(combined_recallBias_data$Accuracy, c(0, 1), labels = c('incorrect', 'correct'))

# Get annotation postions
y_position <- rep(80, 3)
dodgePos   <- 0.375/2
xmin       <- c(1, 2, 3) - dodgePos
xmax       <- c(1, 2, 3) + dodgePos
tip_length <- c(0.1, 0.3)
anno1      <- TeX('$BF_{10} > 100$')

# Plot
biasPlot <- ggplot(combined_recallBias_data, aes(x = Experiment, y = Expectancy, colour = Accuracy)) + 
  geom_boxplot(outlier.shape = NA) + #avoid plotting outliers twice
  geom_point(position = position_jitterdodge()) +
  labs(title = 'Bias towards congruent locations', y = 'Expectancy (normative data)') +
  scale_color_mrc(palette = 'secondary') +
  geom_signif(annotation = '',
              y_position = y_position, 
              xmin = xmin, 
              xmax = xmax, 
              tip_length = tip_length,
              colour = 'black') +
  annotate('text', x = 1, y = 85, label = anno1, size = 3) +
  annotate('text', x = 2, y = 85, label = anno1, size = 3) +
  annotate('text', x = 3, y = 85, label = anno1, size = 3) +
  theme(legend.position = c(0, 0),
       legend.justification = c(0, 0),
       legend.key.size = unit(0.3, "cm"))

save_plot("biasPlot.png", biasPlot,
          base_height = 6/cm(1),
          base_width = 12.66667/cm(1),
          base_aspect_ratio = 1, 
          dpi = 300)