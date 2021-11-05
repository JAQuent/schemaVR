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
path2parent <- "D:/Alex/Laptop/Desktop/schemaVR" # This need to be changed to run this document
######################################################

# Save path2parent to holder variable
path2parent2 <- path2parent

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
drawSamples  <- 1000
lineAlpha    <- 0.01 # Quad plots
lineSize     <- 0.1
se_colour    <- 'red'
loess_colour <- 'red'
steps        <- 20 # steps for kitchen objects raster
y1           <- 0 # y lim
y2           <- 1 # y lim

# Set theme default
updatedTheme <- theme_grey() + theme(axis.title.x  = element_text(size = 10),
                                     axis.title.y  = element_text(size = 8),
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
load(paste0(path2parent, "/schemaVR4/analysis/schemaVR4_recall_sequential_20210708_172952.RData"))
sd_value_recall  <- sd_value
path2parent      <- path2parent2 # Reset path


# Load data for 3AFC
load(paste0(path2parent, "/schemaVR4/analysis/schemaVR4_AFC_sequential_20210709_103522.RData"))
sd_value_AFC     <- sd_value
path2parent      <- path2parent2 # Reset path

# Load data for RK
load(paste0(path2parent, "/schemaVR4/analysis/schemaVR4_RK_sequential_20210709_143030.RData"))
sd_value_RK      <- sd_value
path2parent      <- path2parent2 # Reset path

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
       x = "Expectancy",
       title = 'Recall')
recallPlot1$layers[[1]]$aes_params$colour <- rgb(0, 0, 0, lineAlpha)

# Add Loess smoothing based on the data
recallPlot1 <- recallPlot1 + geom_smooth(data = model_schemaVR1_recall2$data, 
                                         mapping = aes(x = sExp , y = accRecall), 
                                         colour = loess_colour, 
                                         alpha = 0.1, 
                                         method = 'loess', 
                                         fill = se_colour)

# Displaying from 0 to 1 on y axis
recallPlot1 <- recallPlot1 + coord_cartesian(ylim = c(y1, y2), expand = FALSE)  + updatedTheme

# Reorder of layers so that the loess line is in the background
temp1 <- recallPlot1$layers[[1]]
temp2 <- recallPlot1$layers[[2]]
recallPlot1$layers[[1]] <- recallPlot1$layers[[3]]
recallPlot1$layers[[2]] <- temp1
recallPlot1$layers[[3]] <- temp2
recallPlot1


########  AFC
# Plot by using brms function
AFCPlot1 <- plot(marginal_effects(model_schemaVR1_AFC2, 
                                     spaghetti = TRUE, 
                                     nsamples = drawSamples), points = FALSE)

# Extract plot object, add title etc. and change parameters
AFCPlot1 <- AFCPlot1$sExp
AFCPlot1 <- AFCPlot1 + 
  labs(y = 'p(3AFC accuracy)',
       x = "Expectancy",
       title = '3AFC')
AFCPlot1$layers[[1]]$aes_params$colour <- rgb(0, 0, 0, lineAlpha)

# Add Loess smoothing based on the data
AFCPlot1 <- AFCPlot1 + geom_smooth(data = model_schemaVR1_AFC2$data, 
                                   mapping = aes(x = sExp , y = accAFC), 
                                   colour = loess_colour, 
                                   alpha = 0.1, 
                                   method = 'loess', 
                                   fill = se_colour)

# Displaying from 0 to 1 on y axis
AFCPlot1 <- AFCPlot1 + coord_cartesian(ylim = c(y1, y2), expand = FALSE)

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
recallPlot2 <- recallPlot2 + coord_cartesian(ylim = c(y1, y2), expand = FALSE)

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
AFCPlot2 <- AFCPlot2 + coord_cartesian(ylim = c(y1, y2), expand = FALSE)

# Reorder of layers so that the loess line is in the background
temp1 <- AFCPlot2$layers[[1]]
temp2 <- AFCPlot2$layers[[2]]
AFCPlot2$layers[[1]] <- AFCPlot2$layers[[3]]
AFCPlot2$layers[[2]] <- temp1
AFCPlot2$layers[[3]] <- temp2


# /* 
# ----------------------------- schemaVR3 ---------------------------
# */
######## Recall accuracy
# Plot by using brms function
recallPlot3 <- plot(marginal_effects(model_schemaVR3_recall, 
                                     spaghetti = TRUE, 
                                     nsamples = drawSamples), points = FALSE)

# Extract plot object, add title etc. and change parameters
recallPlot3 <- recallPlot3$sExp
recallPlot3 <- recallPlot3 + 
  labs(y = 'p(recall accuracy)',
       x = "Expectancy")
recallPlot3$layers[[1]]$aes_params$colour <- rgb(0, 0, 0, lineAlpha)

# Add Loess smoothing based on the data
recallPlot3 <- recallPlot3 + geom_smooth(data = model_schemaVR3_recall$data, 
                                         mapping = aes(x = sExp , y = accRecall), 
                                         colour = loess_colour, 
                                         alpha = 0.1, 
                                         method = 'loess', 
                                         fill = se_colour)

# Displaying from 0 to 1 on y axis
recallPlot3 <- recallPlot3 + coord_cartesian(ylim = c(y1, y2), expand = FALSE)

# Reorder of layers so that the loess line is in the background
temp1 <- recallPlot3$layers[[1]]
temp2 <- recallPlot3$layers[[2]]
recallPlot3$layers[[1]] <- recallPlot3$layers[[3]]
recallPlot3$layers[[2]] <- temp1
recallPlot3$layers[[3]] <- temp2



########  AFC
# Plot by using brms function
AFCPlot3 <- plot(marginal_effects(model_schemaVR3_AFC, 
                                  spaghetti = TRUE, 
                                  nsamples = drawSamples), points = FALSE)

# Extract plot object, add title etc. and change parameters
AFCPlot3 <- AFCPlot3$sExp
AFCPlot3 <- AFCPlot3 + 
  labs(y = 'p(3AFC accuracy)',
       x = "Expectancy")
AFCPlot3$layers[[1]]$aes_params$colour <- rgb(0, 0, 0, lineAlpha)

# Add Loess smoothing based on the data
AFCPlot3 <- AFCPlot3 + geom_smooth(data = model_schemaVR3_AFC$data, 
                                   mapping = aes(x = sExp , y = accAFC), 
                                   colour = loess_colour, 
                                   alpha = 0.1, 
                                   method = 'loess', 
                                   fill = se_colour)

# Displaying from 0 to 1 on y axis
AFCPlot3 <- AFCPlot3 + coord_cartesian(ylim = c(y1, y2), expand = FALSE)

# Reorder of layers so that the loess line is in the background
temp1 <- AFCPlot3$layers[[1]]
temp2 <- AFCPlot3$layers[[2]]
AFCPlot3$layers[[1]] <- AFCPlot3$layers[[3]]
AFCPlot3$layers[[2]] <- temp1
AFCPlot3$layers[[3]] <- temp2

# /* 
# ----------------------------- schemaVR4 ---------------------------
# */
######## Recall accuracy
# Plot by using brms function
recallPlot4 <- plot(marginal_effects(model_schemaVR4_recall, 
                                     spaghetti = TRUE, 
                                     nsamples = drawSamples), points = FALSE)

# Extract plot object, add title etc. and change parameters
recallPlot4 <- recallPlot4$sExp
recallPlot4 <- recallPlot4 + 
  labs(y = 'p(recall accuracy)',
       x = "Expectancy")
recallPlot4$layers[[1]]$aes_params$colour <- rgb(0, 0, 0, lineAlpha)

# Add Loess smoothing based on the data
recallPlot4 <- recallPlot4 + geom_smooth(data = model_schemaVR4_recall$data, 
                                         mapping = aes(x = sExp , y = accRecall), 
                                         colour = loess_colour, 
                                         alpha = 0.1, 
                                         method = 'loess', 
                                         fill = se_colour)

# Displaying from 0 to 1 on y axis
recallPlot4 <- recallPlot4 + coord_cartesian(ylim = c(y1, y2), expand = FALSE)

# Reorder of layers so that the loess line is in the background
temp1 <- recallPlot4$layers[[1]]
temp2 <- recallPlot4$layers[[2]]
recallPlot4$layers[[1]] <- recallPlot4$layers[[3]]
recallPlot4$layers[[2]] <- temp1
recallPlot4$layers[[3]] <- temp2



########  AFC
# Plot by using brms function
AFCPlot4 <- plot(marginal_effects(model_schemaVR4_AFC, 
                                  spaghetti = TRUE, 
                                  nsamples = drawSamples), points = FALSE)

# Extract plot object, add title etc. and change parameters
AFCPlot4 <- AFCPlot4$sExp
AFCPlot4 <- AFCPlot4 + 
  labs(y = 'p(3AFC accuracy)',
       x = "Expectancy")
AFCPlot4$layers[[1]]$aes_params$colour <- rgb(0, 0, 0, lineAlpha)

# Add Loess smoothing based on the data
AFCPlot4 <- AFCPlot4 + geom_smooth(data = model_schemaVR4_AFC$data, 
                                   mapping = aes(x = sExp , y = accAFC), 
                                   colour = loess_colour, 
                                   alpha = 0.1, 
                                   method = 'loess', 
                                   fill = se_colour,
                                   linetype = 'dashed')

# Displaying from 0 to 1 on y axis
AFCPlot4 <- AFCPlot4 + coord_cartesian(ylim = c(y1, y2), expand = FALSE)

# Reorder of layers so that the loess line is in the background
temp1 <- AFCPlot4$layers[[1]]
temp2 <- AFCPlot4$layers[[2]]
AFCPlot4$layers[[1]] <- AFCPlot4$layers[[3]]
AFCPlot4$layers[[2]] <- temp1
AFCPlot4$layers[[3]] <- temp2

# /* 
# ----------------------------- Get legend ---------------------------
# */
# Create legend with random data
legendData <- data.frame(x = rnorm(10), y = rnorm(10), cond = as.factor(rep(1:2, each = 5)))

legendPlot <- ggplot(legendData, aes(x = x, y = y, colour = cond)) + 
  geom_line(size = 1.5) +
  scale_color_manual(values=c(loess_colour,'White'), labels = c('Loess', 'Model fit')) +
  guides(colour = guide_legend(title = "Legend"))
  
legend <- cowplot::get_legend(legendPlot)


# /* 
# ----------------------------- Combine into 1 figure ---------------------------
# */
# Make 1 figure
figure2 <- plot_grid(recallPlot1, AFCPlot1, 
                     recallPlot2, AFCPlot2, 
                     recallPlot3, AFCPlot3, 
                     recallPlot4, AFCPlot4,
                     ncol = 2, labels = 'AUTO')
# Add legend
figure2 <- plot_grid(figure2, legend, rel_widths = c(3, 1))


# save as image
save_plot("figure2.png", figure2,
          base_height = 18/cm(1),
          base_width = 12/cm(1),
          base_aspect_ratio = 1)


# /* 
# ----------------------------- Figure 3 ---------------------------
# */
# 2 x 3 for remember & familiarity

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
       x = "Expectancy",
       title = 'Recollection')
rememPlot1$layers[[1]]$aes_params$colour <- rgb(0, 0, 0, lineAlpha)

# Add Loess smoothing based on the data
rememPlot1 <- rememPlot1 + geom_smooth(data = model_schemaVR2_rem$data, 
                                       mapping = aes(x = sExp , y = remembered), 
                                       colour = loess_colour, 
                                       alpha = 0.1, 
                                       method = 'loess', 
                                       fill = se_colour)

# Displaying from 0 to 1 on y axis
rememPlot1 <- rememPlot1 + coord_cartesian(ylim = c(y1, y2), expand = FALSE)

# Reorder of layers so that the loess line is in the background
temp1 <- rememPlot1$layers[[1]]
temp2 <- rememPlot1$layers[[2]]
rememPlot1$layers[[1]] <- rememPlot1$layers[[3]]
rememPlot1$layers[[2]] <- temp1
rememPlot1$layers[[3]] <- temp2

######## Familiar
# Plot by using brms function
familiarPlot1 <- plot(marginal_effects(model_schemaVR2_familiar_ind, 
                                       spaghetti = TRUE, 
                                       nsamples = drawSamples), points = FALSE)

# Extract plot object, add title etc. and change parameters
familiarPlot1 <- familiarPlot1$sExp
familiarPlot1 <- familiarPlot1 + 
  labs(y = 'p(Familiarity)',
       x = "Expectancy",
       title = 'Familiarity')
familiarPlot1$layers[[1]]$aes_params$colour <- rgb(0, 0, 0, lineAlpha)

# Add Loess smoothing based on the data
familiarPlot1 <- familiarPlot1 + geom_smooth(data = model_schemaVR2_familiar_ind$data, 
                                             mapping = aes(x = sExp , y = familiar_ind), 
                                             colour = loess_colour, 
                                             alpha = 0.1, 
                                             method = 'loess', 
                                             fill = se_colour)

# Displaying from 0 to 1 on y axis
familiarPlot1 <- familiarPlot1 + coord_cartesian(ylim = c(y1, y2), expand = FALSE)

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
rememPlot2 <- plot(marginal_effects(model_schemaVR3_rem, 
                                    spaghetti = TRUE, 
                                    nsamples = drawSamples), points = FALSE)

# Extract plot object, add title etc. and change parameters
rememPlot2 <- rememPlot2$sExp
rememPlot2 <- rememPlot2 + 
  labs(y = 'p(Recollection)',
       x = "Expectancy")
rememPlot2$layers[[1]]$aes_params$colour <- rgb(0, 0, 0, lineAlpha)

# Add Loess smoothing based on the data
rememPlot2 <- rememPlot2 + geom_smooth(data = model_schemaVR3_rem$data, 
                                       mapping = aes(x = sExp , y = remembered), 
                                       colour = loess_colour, 
                                       alpha = 0.1, 
                                       method = 'loess', 
                                       fill = se_colour)

# Displaying from 0 to 1 on y axis
rememPlot2 <- rememPlot2 + coord_cartesian(ylim = c(y1, y2), expand = FALSE)

# Reorder of layers so that the loess line is in the background
temp1 <- rememPlot2$layers[[1]]
temp2 <- rememPlot2$layers[[2]]
rememPlot2$layers[[1]] <- rememPlot2$layers[[3]]
rememPlot2$layers[[2]] <- temp1
rememPlot2$layers[[3]] <- temp2


######## Familiar
# Plot by using brms function
familiarPlot2 <- plot(marginal_effects(model_schemaVR3_familiar_ind, 
                                       spaghetti = TRUE, 
                                       nsamples = drawSamples), points = FALSE)

# Extract plot object, add title etc. and change parameters
familiarPlot2 <- familiarPlot2$sExp
familiarPlot2 <- familiarPlot2 + 
  labs(y = 'p(Familiarity)',
       x = "Expectancy")
familiarPlot2$layers[[1]]$aes_params$colour <- rgb(0, 0, 0, lineAlpha)

# Add Loess smoothing based on the data
familiarPlot2 <- familiarPlot2 + geom_smooth(data = model_schemaVR3_familiar_ind$data, 
                                             mapping = aes(x = sExp, y = familiar_ind), 
                                             colour = loess_colour, 
                                             alpha = 0.1, 
                                             method = 'loess', 
                                             fill = se_colour)

# Displaying from 0 to 1 on y axis
familiarPlot2 <- familiarPlot2 + coord_cartesian(ylim = c(y1, y2), expand = FALSE)

# Reorder of layers so that the loess line is in the background
temp1 <- familiarPlot2$layers[[1]]
temp2 <- familiarPlot2$layers[[2]]
familiarPlot2$layers[[1]] <- familiarPlot2$layers[[3]]
familiarPlot2$layers[[2]] <- temp1
familiarPlot2$layers[[3]] <- temp2

# /* 
# ----------------------------- schemaVR4 ---------------------------
# */
######## Remember
# Plot by using brms function
rememPlot3 <- plot(marginal_effects(model_schemaVR4_rem, 
                                    spaghetti = TRUE, 
                                    nsamples = drawSamples), points = FALSE)

# Extract plot object, add title etc. and change parameters
rememPlot3 <- rememPlot3$sExp
rememPlot3 <- rememPlot3 + 
  labs(y = 'p(Recollection)',
       x = "Expectancy")
rememPlot3$layers[[1]]$aes_params$colour <- rgb(0, 0, 0, lineAlpha)

# Add Loess smoothing based on the data
rememPlot3 <- rememPlot3 + geom_smooth(data = model_schemaVR4_rem$data, 
                                       mapping = aes(x = sExp , y = remembered), 
                                       colour = loess_colour, 
                                       alpha = 0.1, 
                                       method = 'loess', 
                                       fill = se_colour)

# Displaying from 0 to 1 on y axis
rememPlot3 <- rememPlot3 + coord_cartesian(ylim = c(y1, y2), expand = FALSE)

# Reorder of layers so that the loess line is in the background
temp1 <- rememPlot3$layers[[1]]
temp2 <- rememPlot3$layers[[2]]
rememPlot3$layers[[1]] <- rememPlot3$layers[[3]]
rememPlot3$layers[[2]] <- temp1
rememPlot3$layers[[3]] <- temp2


######## Familiar
# Plot by using brms function
familiarPlot3 <- plot(marginal_effects(model_schemaVR4_familiar_ind, 
                                       spaghetti = TRUE, 
                                       nsamples = drawSamples), points = FALSE)

# Extract plot object, add title etc. and change parameters
familiarPlot3 <- familiarPlot3$sExp
familiarPlot3 <- familiarPlot3 + 
  labs(y = 'p(Familiarity)',
       x = "Expectancy")
familiarPlot3$layers[[1]]$aes_params$colour <- rgb(0, 0, 0, lineAlpha)

# Add Loess smoothing based on the data
familiarPlot3 <- familiarPlot3 + geom_smooth(data = model_schemaVR4_familiar_ind$data, 
                                             mapping = aes(x = sExp, y = familiar_ind), 
                                             colour = loess_colour, 
                                             alpha = 0.1, 
                                             method = 'loess', 
                                             fill = se_colour)

# Displaying from 0 to 1 on y axis
familiarPlot3 <- familiarPlot3 + coord_cartesian(ylim = c(y1, y2), expand = FALSE)

# Reorder of layers so that the loess line is in the background
temp1 <- familiarPlot3$layers[[1]]
temp2 <- familiarPlot3$layers[[2]]
familiarPlot3$layers[[1]] <- familiarPlot3$layers[[3]]
familiarPlot3$layers[[2]] <- temp1
familiarPlot3$layers[[3]] <- temp2

# /* 
# ----------------------------- Combine into 1 figure ---------------------------
# */
# Make 1 figure
figure3 <- plot_grid(rememPlot1, familiarPlot1, 
                     rememPlot2, familiarPlot2, 
                     rememPlot3, familiarPlot3, 
                     ncol = 2, labels = 'AUTO')

# Add legend (made earlier)
figure3 <- plot_grid(figure3, legend, rel_widths = c(3, 1))

# save as image
save_plot("figure3.png", figure3,
          base_height = 16/cm(1),
          base_width =  12/cm(1),
          base_aspect_ratio = 1)


# /* 
# ----------------------------- Supplement: Spread across participants ---------------------------
# */
# Loading data
# Loading all .RData files
load("D:/Alex/Laptop/Desktop/schemaVR/schemaVR1/data/dataSchemaVR1_cleaned.RData")
load("D:/Alex/Laptop/Desktop/schemaVR/schemaVR2/data/dataSchemaVR2_cleaned.RData")
load("D:/Alex/Laptop/Desktop/schemaVR/schemaVR3/data/dataSchemaVR3_cleaned.RData")
load("D:/Alex/Laptop/Desktop/schemaVR/schemaVR4/data/dataSchemaVR4_cleaned.RData")

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
                                             rep('3a', length(dataSchemaVR3$subNum)),
                                             rep('3b', length(dataSchemaVR4$subNum))))
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
