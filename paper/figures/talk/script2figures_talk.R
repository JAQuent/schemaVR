# Script creates figures for a talk in the Shape of U paper
# Version 1.0
# Date: 18/11/2021
# Author: Joern Alexander Quent


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
setwd(paste0(path2parent, "/paper/figures/talk"))

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
       title = 'Experiment 1')
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
       x = "Expectancy",
       title = 'Experiment 2')
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
       x = "Expectancy",
       title = 'Experiment 3a')
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
       x = "Expectancy",
       title = 'Experiment 3b')
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
figure2 <- plot_grid(AFCPlot1, AFCPlot2, AFCPlot3, AFCPlot4,
                     recallPlot1, recallPlot2, recallPlot3, recallPlot4, align = c("hv"),
                     ncol = 4)
# Add legend
figure2 <- plot_grid(figure2, legend, rel_widths = c(3, 1))


# save as image
save_plot("mainResults.png", figure2,
          base_height = 8/cm(1),
          base_width = 18/cm(1),
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
       title = 'Experiment 2')
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
       x = "Expectancy",
       title =  'Experiment 3')
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
       x = "Expectancy",
       title =  'Experiment 3b')
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
# Make 1 figur
figure3 <- plot_grid(rememPlot1, rememPlot2, rememPlot3, 
                     familiarPlot1, familiarPlot2, familiarPlot3, 
                     ncol = 3)

# Add legend (made earlier)
figure3 <- plot_grid(figure3, legend, rel_widths = c(3, 1))

# save as image
save_plot("recFam_results.png", figure3,
          base_height = 8/cm(1),
          base_width =  16/cm(1),
          base_aspect_ratio = 1)

