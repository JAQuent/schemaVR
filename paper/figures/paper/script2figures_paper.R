# Script creates figures for the 'Shape of U: The non-monotonic relationship between object-location memory and expectedness' paper
# Version 3.0
# Date: 21/01/2022
# Author: Joern Alexander Quent

# To do cap the plots correctly 


# /* 
# ----------------------------- Libraries, settings and functions ---------------------------
# */
######################################################
# Path to parent folder schemaVR
path2parent <- "E:/Alex/research_projects/schemaVR/schemaVR" # This need to be changed to run this document
######################################################

# Save path2parent to holder variable
path2parent3 <- path2parent

# Setting wd
setwd(paste0(path2parent, "/paper/figures/paper"))

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
pointAlpha   <- 0.05
point_colour <- 'black'
xTitle       <- "Unxpected      Expected"

# Set theme default
updatedTheme <- theme_grey() + theme(axis.title.x  = element_text(size = 10),
                                     axis.title.y  = element_text(size = 10),
                                     axis.text.y  = element_text(size = 5),
                                     axis.text.x  = element_text(size = 5),
                                     plot.title = element_text(size = 10))

theme_set(updatedTheme)
# see also here https://rpubs.com/Koundy/71792
# see also here http://sape.inf.usi.ch/quick-reference/ggplot2/themes
# see also http://www.sthda.com/english/articles/24-ggpubr-publication-ready-plots/

# /* 
# ----------------------------- Load all models ---------------------------
# */

# Load data for recall
load(paste0(path2parent, "/schemaVR4/analysis/schemaVR4_AFC_sequential_20220206_135656.RData"))
path2parent <- path2parent3

# Load data for 3AFC
load(paste0(path2parent, "/schemaVR4/analysis/schemaVR4_recall_sequential_20220206_154319.RData"))
path2parent <- path2parent3

# Load data for RK
load(paste0(path2parent, "/schemaVR4/analysis/schemaVR4_RK_sequential_20220207_141708.RData"))
path2parent <- path2parent3

# /* 
# ----------------------------- Figure 2 ---------------------------
# */
# /* 
# ----------------------------- schemaVR1 ---------------------------
# */
########  Recall accuracy
# Plot by using brms function
tempPlot <- plot(conditional_effects(model_schemaVR1_recall))

# Extract plot object, add title etc. and change parameters
tempPlot <- tempPlot$Exp # Just extract the plot 'layer' if that what you call it. 
tempPlot <- tempPlot + 
  labs(y = 'p(recall accuracy)',
       x = xTitle,
       title = 'Recall')

# Add the points to the plot
tempPlot <- tempPlot + geom_point(data = model_schemaVR1_recall$data, 
                                  mapping = aes(x = Exp , y = accRecall), 
                                  colour = point_colour , 
                                  alpha = pointAlpha, inherit.aes = FALSE)

# Assign to correct variable name
recallPlot1 <- tempPlot


########  AFC
# Plot by using brms function
tempPlot <- plot(conditional_effects(model_schemaVR1_AFC))

# Extract plot object, add title etc. and change parameters
tempPlot <- tempPlot$Exp # Just extract the plot 'layer' if that what you call it. 
tempPlot <- tempPlot + 
  labs(y = 'p(3AFC accuracy)',
       x = xTitle,
       title = '3AFC')

# Add the points to the plot
tempPlot <- tempPlot + geom_point(data = model_schemaVR1_AFC$data, 
                                   mapping = aes(x = Exp , y = accAFC), 
                                   colour = point_colour , 
                                   alpha = pointAlpha, inherit.aes = FALSE)

# Assign to correct variable name
AFCPlot1 <- tempPlot

# /* 
# ----------------------------- schemaVR2 ---------------------------
# */
########  Recall accuracy
# Plot by using brms function
tempPlot <- plot(conditional_effects(model_schemaVR2_recall))

# Extract plot object, add title etc. and change parameters
tempPlot <- tempPlot$Exp # Just extract the plot 'layer' if that what you call it. 
tempPlot <- tempPlot + 
  labs(y = 'p(recall accuracy)',
       x = xTitle)

# Add the points to the plot
tempPlot <- tempPlot + geom_point(data = model_schemaVR2_recall$data, 
                                  mapping = aes(x = Exp , y = accRecall), 
                                  colour = point_colour , 
                                  alpha = pointAlpha, inherit.aes = FALSE)

# Assign to correct variable name
recallPlot2 <- tempPlot

########  AFC
# Plot by using brms function
tempPlot <- plot(conditional_effects(model_schemaVR2_AFC))

# Extract plot object, add title etc. and change parameters
tempPlot <- tempPlot$Exp # Just extract the plot 'layer' if that what you call it. 
tempPlot <- tempPlot + 
  labs(y = 'p(3AFC accuracy)',
       x = xTitle)

# Add the points to the plot
tempPlot <- tempPlot + geom_point(data = model_schemaVR2_AFC$data, 
                                  mapping = aes(x = Exp , y = accAFC), 
                                  colour = point_colour , 
                                  alpha = pointAlpha, inherit.aes = FALSE)

# Assign to correct variable name
AFCPlot2 <- tempPlot

# /* 
# ----------------------------- schemaVR3 ---------------------------
# */
########  Recall accuracy
# Plot by using brms function
tempPlot <- plot(conditional_effects(model_schemaVR3_recall))

# Extract plot object, add title etc. and change parameters
tempPlot <- tempPlot$Exp # Just extract the plot 'layer' if that what you call it. 
tempPlot <- tempPlot + 
  labs(y = 'p(recall accuracy)',
       x = xTitle)

# Add the points to the plot
tempPlot <- tempPlot + geom_point(data = model_schemaVR3_recall$data, 
                                  mapping = aes(x = Exp , y = accRecall), 
                                  colour = point_colour , 
                                  alpha = pointAlpha, inherit.aes = FALSE)

# Assign to correct variable name
recallPlot3 <- tempPlot

########  AFC
# Plot by using brms function
tempPlot <- plot(conditional_effects(model_schemaVR3_AFC))

# Extract plot object, add title etc. and change parameters
tempPlot <- tempPlot$Exp # Just extract the plot 'layer' if that what you call it. 
tempPlot <- tempPlot + 
  labs(y = 'p(3AFC accuracy)',
       x = xTitle)

# Add the points to the plot
tempPlot <- tempPlot + geom_point(data = model_schemaVR3_AFC$data, 
                                  mapping = aes(x = Exp , y = accAFC), 
                                  colour = point_colour , 
                                  alpha = pointAlpha, inherit.aes = FALSE)

# Assign to correct variable name
AFCPlot3 <- tempPlot

# /* 
# ----------------------------- schemaVR4 ---------------------------
# */
########  Recall accuracy
# Plot by using brms function
tempPlot <- plot(conditional_effects(model_schemaVR4_recall))

# Extract plot object, add title etc. and change parameters
tempPlot <- tempPlot$Exp # Just extract the plot 'layer' if that what you call it. 
tempPlot <- tempPlot + 
  labs(y = 'p(recall accuracy)',
       x = xTitle)

# Add the points to the plot
tempPlot <- tempPlot + geom_point(data = model_schemaVR4_recall$data, 
                                  mapping = aes(x = Exp , y = accRecall), 
                                  colour = point_colour , 
                                  alpha = pointAlpha, inherit.aes = FALSE)

# Assign to correct variable name
recallPlot4 <- tempPlot

########  AFC
# Plot by using brms function
tempPlot <- plot(conditional_effects(model_schemaVR4_AFC))

# Extract plot object, add title etc. and change parameters
tempPlot <- tempPlot$Exp # Just extract the plot 'layer' if that what you call it. 
tempPlot <- tempPlot + 
  labs(y = 'p(3AFC accuracy)',
       x = xTitle)

# Add the points to the plot
tempPlot <- tempPlot + geom_point(data = model_schemaVR4_AFC$data, 
                                  mapping = aes(x = Exp , y = accAFC), 
                                  colour = point_colour , 
                                  alpha = pointAlpha, inherit.aes = FALSE)

# Assign to correct variable name
AFCPlot4 <- tempPlot


# /* 
# ----------------------------- Combine into 1 figure ---------------------------
# */
# Make 1 figure
figure2 <- plot_grid(recallPlot1, AFCPlot1, 
                     recallPlot2, AFCPlot2, 
                     recallPlot3, AFCPlot3, 
                     recallPlot4, AFCPlot4,
                     ncol = 2, labels = 'AUTO', align = 'hv')


# save as image
save_plot("figure2.png", figure2,
          base_height = 18/cm(1),
          base_width = 11/cm(1),
          base_aspect_ratio = 1,
          dpi = 300)


# /* 
# ----------------------------- Figure 3 ---------------------------
# */
# 2 x 3 for remember & familiarity

# /* 
# ----------------------------- schemaVR2 ---------------------------
# */
########  Remember
# Plot by using brms function
tempPlot <- plot(conditional_effects(model_schemaVR2_rem))

# Extract plot object, add title etc. and change parameters
tempPlot <- tempPlot$Exp # Just extract the plot 'layer' if that what you call it. 
tempPlot <- tempPlot + 
  labs(y = 'p(remember)',
       x = xTitle,
       title = 'Recollection')

# Add the points to the plot
tempPlot <- tempPlot + geom_point(data = model_schemaVR2_rem$data, 
                                  mapping = aes(x = Exp , y = remembered), 
                                  colour = point_colour , 
                                  alpha = pointAlpha, inherit.aes = FALSE)

# Assign to correct variable name
rememPlot1 <- tempPlot

########  Familiar
# Plot by using brms function
tempPlot <- plot(conditional_effects(model_schemaVR2_familiar_ind))

# Extract plot object, add title etc. and change parameters
tempPlot <- tempPlot$Exp # Just extract the plot 'layer' if that what you call it. 
tempPlot <- tempPlot + 
  labs(y = 'p(familiar)',
       x = xTitle,
       title = 'Familiarity')

# Add the points to the plot
tempPlot <- tempPlot + geom_point(data = model_schemaVR2_familiar_ind$data, 
                                  mapping = aes(x = Exp , y = familiar_ind), 
                                  colour = point_colour , 
                                  alpha = pointAlpha, inherit.aes = FALSE)

# Assign to correct variable name
familiarPlot1 <- tempPlot


# /* 
# ----------------------------- schemaVR3 ---------------------------
# */
########  Remember
# Plot by using brms function
tempPlot <- plot(conditional_effects(model_schemaVR3_rem))

# Extract plot object, add title etc. and change parameters
tempPlot <- tempPlot$Exp # Just extract the plot 'layer' if that what you call it. 
tempPlot <- tempPlot + 
  labs(y = 'p(remember)',
       x = xTitle)

# Add the points to the plot
tempPlot <- tempPlot + geom_point(data = model_schemaVR3_rem$data, 
                                  mapping = aes(x = Exp , y = remembered), 
                                  colour = point_colour , 
                                  alpha = pointAlpha, inherit.aes = FALSE)

# Assign to correct variable name
rememPlot2 <- tempPlot

########  Familiar
# Plot by using brms function
tempPlot <- plot(conditional_effects(model_schemaVR3_familiar_ind))

# Extract plot object, add title etc. and change parameters
tempPlot <- tempPlot$Exp # Just extract the plot 'layer' if that what you call it. 
tempPlot <- tempPlot + 
  labs(y = 'p(familiar)',
       x = xTitle)

# Add the points to the plot
tempPlot <- tempPlot + geom_point(data = model_schemaVR3_familiar_ind$data, 
                                  mapping = aes(x = Exp , y = familiar_ind), 
                                  colour = point_colour , 
                                  alpha = pointAlpha, inherit.aes = FALSE)

# Assign to correct variable name
familiarPlot2 <- tempPlot

# /* 
# ----------------------------- schemaVR4 ---------------------------
# */
########  Remember
# Plot by using brms function
tempPlot <- plot(conditional_effects(model_schemaVR4_rem))

# Extract plot object, add title etc. and change parameters
tempPlot <- tempPlot$Exp # Just extract the plot 'layer' if that what you call it. 
tempPlot <- tempPlot + 
  labs(y = 'p(remember)',
       x = xTitle)

# Add the points to the plot
tempPlot <- tempPlot + geom_point(data = model_schemaVR4_rem$data, 
                                  mapping = aes(x = Exp , y = remembered), 
                                  colour = point_colour , 
                                  alpha = pointAlpha, inherit.aes = FALSE)

# Assign to correct variable name
rememPlot3 <- tempPlot

########  Familiar
# Plot by using brms function
tempPlot <- plot(conditional_effects(model_schemaVR4_familiar_ind))

# Extract plot object, add title etc. and change parameters
tempPlot <- tempPlot$Exp # Just extract the plot 'layer' if that what you call it. 
tempPlot <- tempPlot + 
  labs(y = 'p(familiar)',
       x = xTitle)

# Add the points to the plot
tempPlot <- tempPlot + geom_point(data = model_schemaVR4_familiar_ind$data, 
                                  mapping = aes(x = Exp , y = familiar_ind), 
                                  colour = point_colour , 
                                  alpha = pointAlpha, inherit.aes = FALSE)

# Assign to correct variable name
familiarPlot3 <- tempPlot


# /* 
# ----------------------------- Combine into 1 figure ---------------------------
# */
# Make 1 figure
figure3 <- plot_grid(rememPlot1, familiarPlot1, 
                     rememPlot2, familiarPlot2, 
                     rememPlot3, familiarPlot3, 
                     ncol = 2, labels = 'AUTO', align = 'hv')


# save as image
save_plot("figure3.png", figure3,
          base_height = 16/cm(1),
          base_width =  11/cm(1),
          base_aspect_ratio = 1)


# /* 
# ----------------------------- Supplement: Spread across participants ---------------------------
# */
# Loading data
# Loading all .RData files
load("E:/Alex/Laptop/Desktop/schemaVR/schemaVR1/data/dataSchemaVR1_cleaned.RData")
load("E:/Alex/Laptop/Desktop/schemaVR/schemaVR2/data/dataSchemaVR2_cleaned.RData")
load("E:/Alex/Laptop/Desktop/schemaVR/schemaVR3/data/dataSchemaVR3_cleaned.RData")
load("E:/Alex/Laptop/Desktop/schemaVR/schemaVR4/data/dataSchemaVR4_cleaned.RData")

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
                              Experiment = c(rep(1, length(dataSchemaVR1$subNum)),
                                             rep(2, length(dataSchemaVR2$subNum)),
                                             rep(3, length(dataSchemaVR3$subNum)),
                                             rep(4, length(dataSchemaVR4$subNum))))

combinedRatings$Experiment <- factor(combinedRatings$Experiment, levels = 1:4, labels = c('Pilot', '1', '2a', '2b'))

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



# /* 
# ----------------------------- Rating plot ---------------------------
# */
library(gridExtra)
theme_set(theme_gray())

# Prepare
subNo           <- 1:6
N               <- length(subNo)
locationRatings <- array(data = NA, dim = c(20, 20, N))
objectRatings   <- matrix(NA, 20, N)

# Sequently loading data
for(i in 1:N){
  locationRatings[,,i] <- matrix(scan(paste0(path2parent,'/normativeStudy/data/locationRatings_', as.character(subNo[i]) ,'.dat')), byrow = TRUE, ncol = 20)
  objectRatings[,i]    <- scan(paste0(path2parent,'/normativeStudy/data/objectRatings_', as.character(subNo[i]) ,'.dat'))
}


objectNames <- c('microwave','kitchen roll','saucepan', 'toaster','fruit bowl','tea pot','knife','mixer','bread','glass jug','mug','dishes','towels','toy','pile of books','umbrella','hat','helmet','calendar','fan')

# Calculate metrics
averageLocationRatings <- apply(locationRatings, 1:2, mean)
sdLocationRatings      <- apply(locationRatings, 1:2, sd)
averageObjectRatings   <- apply(objectRatings, 1, mean)
sdObjectRatings        <- apply(objectRatings, 1, sd)

# Create dataframes to plot
averageLocationRatingsDF <- data.frame(Object = rep(factor(1:20, labels = objectNames), 20), Location = as.factor(rep(1:20, each = 20)), Ratings = c(averageLocationRatings))
sdLocationRatingsDF      <- data.frame(Object = rep(factor(1:20, labels = objectNames), 20), Location = as.factor(rep(1:20, each = 20)), SD = c(sdLocationRatings))

averageObjectRatingsDF  <- data.frame(Object = factor(1:20, labels = objectNames), Ratings = c(averageObjectRatings))
sdObjectRatingsDF       <- data.frame(Object = factor(1:20, labels = objectNames), SD = c(sdObjectRatings))

# Create average heatmaps
plot1 <- ggplot(averageLocationRatingsDF, aes(Location, Object)) + geom_tile(aes(fill = Ratings)) + scale_fill_gradient(low = "white", high = "red", limits = c(-100, 100)) + scale_x_discrete(expand = c(0, 0)) +  scale_y_discrete(expand = c(0, 0)) + labs(title = 'Average: Object/location') + 
  theme(legend.position = 'none')

plot2 <- ggplot(averageObjectRatingsDF, aes(x = 1, y = Object)) + geom_tile(aes(fill = Ratings)) + scale_fill_gradient(low = "white", high = "red", limits = c(-100, 100), breaks = c(50, 0, -50)) + scale_x_discrete(expand = c(0, 0)) +  scale_y_discrete(expand = c(0, 0)) + labs(x = '', title = 'Overall') + theme(axis.title.y = element_blank(), axis.text.y = element_blank(), axis.ticks.y = element_blank(), plot.margin=unit(c(0.2, 0, 0.62, 0),"cm"))


rating_figure <- arrangeGrob(plot1, plot2, ncol = 2, widths = c(5.5, 1))


save_plot("rating_figure.png", rating_figure,
          base_height = 12/cm(1),
          base_width =  18/cm(1),
          base_aspect_ratio = 1)


# /* 
# ----------------------------- Guess results  ---------------------------
# */
########  Guess
# Plot by using brms function
tempPlot <- plot(conditional_effects(model_schemaVRall_guess))

# Extract plot object, add title etc. and change parameters
tempPlot <- tempPlot$Exp # Just extract the plot 'layer' if that what you call it. 
tempPlot <- tempPlot + 
  labs(y = 'p(guess)',
       x = xTitle,
       title = 'Guess')

# Add the points to the plot
tempPlot <- tempPlot + geom_point(data = model_schemaVRall_guess$data, 
                                  mapping = aes(x = Exp , y = guess), 
                                  colour = point_colour , 
                                  alpha = pointAlpha, inherit.aes = FALSE)

# Assign to correct variable name
guessPlot <- tempPlot


save_plot("guessPlot.png", guessPlot,
          base_height = 6/cm(1),
          base_width =  6/cm(1),
          base_aspect_ratio = 1)
