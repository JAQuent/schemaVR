# Script creates figures for the schemaVR paper
# Version 1.0
# Date: 19/11/2019
# Author: Joern Alexander Quent
# /* 
# ----------------------------- Libraries, settings and functions ---------------------------
# */
# Setting wd
setwd("U:/Projects/schemaVR/schemaVR_paper/figures")

# Load data 
load("U:/Projects/schemaVR/schemaVR_paper/scriptsOutput/combinedModels.RData")

# Libraries
library(cowplot)
theme_set(theme_grey())
library(ggplot2)
library(MRColour)
library(assortedRFunctions)
library(brms)
library(plyr)
library(latex2exp)
library(polspline)
library(ggsignif)

# Questions:
# 1. Should I colour which object is a kitchen item?
# 2. Loess line?

# Sizing info: https://www.elsevier.com/authors/author-schemas/artwork-and-media-instructions/artwork-sizing

# Parameters across plots
drawSamples  <- 1000
pointSize    <- 1
lineAlpha    <- 0.01 # Quad plots
lineAlpha2   <- 0.01 # Two Line plots
pointAlpha   <- 0.1
jitterHeight <- 0.1
lineSize     <- 0.1

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
# ----------------------------- Figure 1 Panel A ---------------------------
# */
######## Recall accuracy
# Calculate BF
# Extracting posterior distribution
postDist   <- posterior_samples(schemaVR1_recall_uniformed_randomIntercept)$b_IsExpMUsExp

# Wagenmakers et al. (2010) order-restricted method 1 based on renormalisation: beta = 0 vs. beta > 0 
### Code pasted and adapted from Wagenmakers et al. (2010) found here: http://www.ejwagenmakers.com/papers.html
fit.posterior <- logspline(postDist)

# Normalising by areas of posterior and prior that are included in the intervall to get density of 1
areaPosterior <- sum(postDist > 0)/length(postDist)
posterior     <- dlogspline(0, fit.posterior) 
posterior.OR  <- posterior/areaPosterior    
areaPrior     <- integrate(dstudent_t, 
                           mu = 0,
                           df = 3, 
                           sigma = 2.5, 
                           lower = 0, 
                           upper = Inf, 
                           abs.tol = 0)$value 
prior.OR      <- dstudent_t(0 , 3, 0, 2.5)/areaPrior
bf1           <- round(prior.OR/posterior.OR, 2)

# Plot
recallPlot1 <- plot(marginal_effects(schemaVR1_recall_uniformed_randomIntercept, 
                                     spaghetti = TRUE, 
                                     nsamples = drawSamples), 
                   points = TRUE, 
                   point_args = c(alpha = pointAlpha, size = pointSize),
                   jitter_width = 0)

recallPlot1 <- recallPlot1$sExp
recallPlot1 <- recallPlot1 + 
  labs(y = 'Pr (recall accuracy)',
       x = "Expectancy",
       title = TeX(paste0('Recall accuracy (BF_{10} = ', bf1, ')')))
recallPlot1$layers[[1]]$position <- position_jitter(height = jitterHeight)
recallPlot1$layers[[2]]$aes_params$colour <- rgb(0, 0, 0, lineAlpha)

########  Euclidean distance
# Calculate BF
# Extracting posterior distribution
postDist   <- posterior_samples(schemaVR1_euclid_uniformed_randomIntercept)$b_IsExpMUsExp

# Wagenmakers et al. (2010) order-restricted method 1 based on renormalisation: beta = 0 vs. beta > 0 
### Code pasted and adapted from Wagenmakers et al. (2010) found here: http://www.ejwagenmakers.com/papers.html
fit.posterior <- logspline(postDist)

# Normalising by areas of posterior and prior that are included in the intervall to get density of 1
areaPosterior <- sum(postDist < 0)/length(postDist)
posterior     <- dlogspline(0, fit.posterior) 
posterior.OR  <- posterior/areaPosterior    
areaPrior     <- integrate(dnorm, 
                           mean = 0,
                           sd = 1, 
                           lower = -Inf, 
                           upper = 0, 
                           abs.tol = 0)$value 
prior.OR      <- dnorm(0, 0 , 1)/areaPrior
bf2           <- round(prior.OR/posterior.OR, 2)

# Plot
euclidPlot1 <- plot(marginal_effects(schemaVR1_euclid_uniformed_randomIntercept, 
                                     spaghetti = TRUE, 
                                     nsamples = drawSamples), 
                    points = TRUE,
                    point_args = c(alpha = pointAlpha, size = pointSize))
euclidPlot1 <- euclidPlot1$sExp 
euclidPlot1 <- euclidPlot1 + 
  labs(y = 'Euclidean distance in vm',
       x = "Expectancy",
       title = TeX(paste0('Euclidean distance (BF_{10} = ', bf2, ')')))
euclidPlot1$layers[[2]]$aes_params$colour <- rgb(0, 0, 0, lineAlpha)

########  AFC
# Calculate BF
# Extracting posterior distribution
postDist   <- posterior_samples(schemaVR1_AFC_uniformed_randomIntercept)$b_IsExpMUsExp

# Wagenmakers et al. (2010) order-restricted method 1 based on renormalisation: beta = 0 vs. beta > 0 
### Code pasted and adapted from Wagenmakers et al. (2010) found here: http://www.ejwagenmakers.com/papers.html
fit.posterior <- logspline(postDist)

# Normalising by areas of posterior and prior that are included in the intervall to get density of 1
areaPosterior <- sum(postDist > 0)/length(postDist)
posterior     <- dlogspline(0, fit.posterior) 
posterior.OR  <- posterior/areaPosterior    
areaPrior     <- integrate(dstudent_t, 
                           mu = 0,
                           df = 3, 
                           sigma = 2.5, 
                           lower = 0, 
                           upper = Inf, 
                           abs.tol = 0)$value 
prior.OR      <- dstudent_t(0 , 3, 0, 2.5)/areaPrior
bf3           <- round(prior.OR/posterior.OR, 2)

# Plot
AFCPlot1 <- plot(marginal_effects(schemaVR1_AFC_uniformed_randomIntercept, 
                                  spaghetti = TRUE, 
                                  nsamples = drawSamples), points = TRUE, 
                point_args = c(alpha = pointAlpha, size = pointSize))
AFCPlot1 <- AFCPlot1$sExp 
AFCPlot1 <- AFCPlot1 + 
  labs(y = 'Pr (3AFC accuracy)',
       x = "Expectancy",
       title = TeX(paste0('3AFC accuracy (BF_{10} = ', bf3, ')')))
AFCPlot1$layers[[1]]$position <- position_jitter(height = jitterHeight)
AFCPlot1$layers[[2]]$aes_params$colour <- rgb(0, 0, 0, lineAlpha)

# Combine to 1 panel
panel_A <- plot_grid(recallPlot1, euclidPlot1, AFCPlot1, ncol = 3)

# /* 
# ----------------------------- Figure 1 Panel B ---------------------------
# */
######## Recall accuracy
# Calculate BF
# Extracting posterior distribution
postDist   <- posterior_samples(schemaVR2_recall_uniformed)$b_IsExpMUsExp

# Wagenmakers et al. (2010) order-restricted method 1 based on renormalisation: beta = 0 vs. beta > 0 
### Code pasted and adapted from Wagenmakers et al. (2010) found here: http://www.ejwagenmakers.com/papers.html
fit.posterior <- logspline(postDist)

# Normalising by areas of posterior and prior that are included in the intervall to get density of 1
areaPosterior <- sum(postDist > 0)/length(postDist)
posterior     <- dlogspline(0, fit.posterior) 
posterior.OR  <- posterior/areaPosterior    
areaPrior     <- integrate(dstudent_t, 
                           mu = 0,
                           df = 3, 
                           sigma = 2.5, 
                           lower = 0, 
                           upper = Inf, 
                           abs.tol = 0)$value 
prior.OR      <- dstudent_t(0 , 3, 0, 2.5)/areaPrior
bf1           <- round(prior.OR/posterior.OR, 2)

# Plot
recallPlot2 <- plot(marginal_effects(schemaVR2_recall_uniformed, 
                                     spaghetti = TRUE, 
                                     nsamples = drawSamples), 
                    points = TRUE, 
                    point_args = c(alpha = pointAlpha, size = pointSize),
                    jitter_width = 0)

recallPlot2 <- recallPlot2$sExp
recallPlot2 <- recallPlot2 + 
  labs(y = 'Pr (recall accuracy)',
       x = "Expectancy",
       title = TeX(paste0('Recall accuracy (BF_{10} = ', bf1, ')')))
recallPlot2$layers[[1]]$position <- position_jitter(height = jitterHeight)
recallPlot2$layers[[2]]$aes_params$colour <- rgb(0, 0, 0, lineAlpha)

########  Euclidean distance
# Calculate BF
# Extracting posterior distribution
postDist   <- posterior_samples(schemaVR2_euclid_uniformed)$b_IsExpMUsExp

# Wagenmakers et al. (2010) order-restricted method 1 based on renormalisation: beta = 0 vs. beta > 0 
### Code pasted and adapted from Wagenmakers et al. (2010) found here: http://www.ejwagenmakers.com/papers.html
fit.posterior <- logspline(postDist)

# Normalising by areas of posterior and prior that are included in the intervall to get density of 1
areaPosterior <- sum(postDist < 0)/length(postDist)
posterior     <- dlogspline(0, fit.posterior) 
posterior.OR  <- posterior/areaPosterior    
areaPrior     <- integrate(dnorm, 
                           mean = 0,
                           sd = 1, 
                           lower = -Inf, 
                           upper = 0, 
                           abs.tol = 0)$value 
prior.OR      <- dnorm(0, 0 , 1)/areaPrior
bf2           <- round(prior.OR/posterior.OR, 2)

# Plot
euclidPlot2 <- plot(marginal_effects(schemaVR2_euclid_uniformed, 
                                     spaghetti = TRUE, 
                                     nsamples = drawSamples), 
                    points = TRUE,
                    point_args = c(alpha = pointAlpha, size = pointSize))
euclidPlot2 <- euclidPlot2$sExp 
euclidPlot2 <- euclidPlot2 + 
  labs(y = 'Euclidean distance in vm',
       x = "Expectancy",
       title = TeX(paste0('Euclidean distance (BF_{10} = ', bf2, ')'))) 
euclidPlot2$layers[[2]]$aes_params$colour <- rgb(0, 0, 0, lineAlpha)

########  AFC
# Calculate BF
# Extracting posterior distribution
postDist   <- posterior_samples(schemaVR2_AFC_uniformed)$b_IsExpMUsExp

# Wagenmakers et al. (2010) order-restricted method 1 based on renormalisation: beta = 0 vs. beta > 0 
### Code pasted and adapted from Wagenmakers et al. (2010) found here: http://www.ejwagenmakers.com/papers.html
fit.posterior <- logspline(postDist)

# Normalising by areas of posterior and prior that are included in the intervall to get density of 1
areaPosterior <- sum(postDist > 0)/length(postDist)
posterior     <- dlogspline(0, fit.posterior) 
posterior.OR  <- posterior/areaPosterior    
areaPrior     <- integrate(dstudent_t, 
                           mu = 0,
                           df = 3, 
                           sigma = 2.5, 
                           lower = 0, 
                           upper = Inf, 
                           abs.tol = 0)$value 
prior.OR      <- dstudent_t(0 , 3, 0, 2.5)/areaPrior
bf3           <- round(prior.OR/posterior.OR, 2)

# Plot
AFCPlot2 <- plot(marginal_effects(schemaVR2_AFC_uniformed, 
                                  spaghetti = TRUE, 
                                  nsamples = drawSamples), points = TRUE, 
                 point_args = c(alpha = pointAlpha, size = pointSize))
AFCPlot2 <- AFCPlot2$sExp 
AFCPlot2 <- AFCPlot2 + 
  labs(y = 'Pr (3AFC accuracy)',
       x = "Expectancy",
       title = TeX(paste0('3AFC accuracy (BF_{10} = ', bf3, ')')))
AFCPlot2$layers[[1]]$position <- position_jitter(height = jitterHeight)
AFCPlot2$layers[[2]]$aes_params$colour <- rgb(0, 0, 0, lineAlpha)

# Combine to 1 panel
panel_B <- plot_grid(recallPlot2, euclidPlot2, AFCPlot2, ncol = 3)

# /* 
# ----------------------------- Figure 1 Panel C ---------------------------
# */
######## Recall accuracy
# Calculate BF
# Extracting posterior distribution
postDist   <- posterior_samples(schemaVR3_recall_uniformed)$b_IsExpMUsExp

# Wagenmakers et al. (2010) order-restricted method 1 based on renormalisation: beta = 0 vs. beta > 0 
### Code pasted and adapted from Wagenmakers et al. (2010) found here: http://www.ejwagenmakers.com/papers.html
fit.posterior <- logspline(postDist)

# Normalising by areas of posterior and prior that are included in the intervall to get density of 1
areaPosterior <- sum(postDist > 0)/length(postDist)
posterior     <- dlogspline(0, fit.posterior) 
posterior.OR  <- posterior/areaPosterior    
areaPrior     <- integrate(dstudent_t, 
                           mu = 0,
                           df = 3, 
                           sigma = 2.5, 
                           lower = 0, 
                           upper = Inf, 
                           abs.tol = 0)$value 
prior.OR      <- dstudent_t(0 , 3, 0, 2.5)/areaPrior
bf1           <- round(prior.OR/posterior.OR, 2)

# Plot
recallPlot3 <- plot(marginal_effects(schemaVR3_recall_uniformed, 
                                     spaghetti = TRUE, 
                                     nsamples = drawSamples), 
                    points = TRUE, 
                    point_args = c(alpha = pointAlpha, size = pointSize),
                    jitter_width = 0)

recallPlot3 <- recallPlot3$sExp
recallPlot3 <- recallPlot3 + 
  labs(y = 'Pr (recall accuracy)',
       x = "Expectancy",
       title = TeX(paste0('Recall accuracy (BF_{10} = ', bf1, ')')))
recallPlot3$layers[[1]]$position <- position_jitter(height = jitterHeight)
recallPlot3$layers[[2]]$aes_params$colour <- rgb(0, 0, 0, lineAlpha)

########  Euclidean distance
# Calculate BF
# Extracting posterior distribution
postDist   <- posterior_samples(schemaVR3_euclid_uniformed)$b_IsExpMUsExp

# Wagenmakers et al. (2010) order-restricted method 1 based on renormalisation: beta = 0 vs. beta > 0 
### Code pasted and adapted from Wagenmakers et al. (2010) found here: http://www.ejwagenmakers.com/papers.html
fit.posterior <- logspline(postDist)

# Normalising by areas of posterior and prior that are included in the intervall to get density of 1
areaPosterior <- sum(postDist < 0)/length(postDist)
posterior     <- dlogspline(0, fit.posterior) 
posterior.OR  <- posterior/areaPosterior    
areaPrior     <- integrate(dnorm, 
                           mean = 0,
                           sd = 1, 
                           lower = -Inf, 
                           upper = 0, 
                           abs.tol = 0)$value 
prior.OR      <- dnorm(0, 0 , 1)/areaPrior
bf2           <- round(prior.OR/posterior.OR, 2)

# Plot
euclidPlot3 <- plot(marginal_effects(schemaVR3_euclid_uniformed, 
                                     spaghetti = TRUE, 
                                     nsamples = drawSamples), 
                    points = TRUE,
                    point_args = c(alpha = pointAlpha, size = pointSize))
euclidPlot3 <- euclidPlot3$sExp 
euclidPlot3 <- euclidPlot3 + 
  labs(y = 'Euclidean distance in vm',
       x = "Expectancy",
       title = TeX(paste0('Euclidean distance (BF_{10} = ', bf2, ')')))
euclidPlot3$layers[[2]]$aes_params$colour <- rgb(0, 0, 0, lineAlpha)

########  AFC
# Calculate BF
# Extracting posterior distribution
postDist   <- posterior_samples(schemaVR3_AFC_uniformed)$b_IsExpMUsExp

# Wagenmakers et al. (2010) order-restricted method 1 based on renormalisation: beta = 0 vs. beta > 0 
### Code pasted and adapted from Wagenmakers et al. (2010) found here: http://www.ejwagenmakers.com/papers.html
fit.posterior <- logspline(postDist)

# Normalising by areas of posterior and prior that are included in the intervall to get density of 1
areaPosterior <- sum(postDist > 0)/length(postDist)
posterior     <- dlogspline(0, fit.posterior) 
posterior.OR  <- posterior/areaPosterior    
areaPrior     <- integrate(dstudent_t, 
                           mu = 0,
                           df = 3, 
                           sigma = 2.5, 
                           lower = 0, 
                           upper = Inf, 
                           abs.tol = 0)$value 
prior.OR      <- dstudent_t(0 , 3, 0, 2.5)/areaPrior
bf3           <- round(prior.OR/posterior.OR, 2)

# Plot
AFCPlot3 <- plot(marginal_effects(schemaVR3_AFC_uniformed, 
                                  spaghetti = TRUE, 
                                  nsamples = drawSamples), points = TRUE, 
                 point_args = c(alpha = pointAlpha, size = pointSize))
AFCPlot3 <- AFCPlot3$sExp 
AFCPlot3 <- AFCPlot3 + 
  labs(y = 'Pr (3AFC accuracy)',
       x = "Expectancy",
       title = TeX(paste0('3AFC accuracy (BF_{10} = ', bf3, ')')))
AFCPlot3$layers[[1]]$position <- position_jitter(height = jitterHeight)
AFCPlot3$layers[[2]]$aes_params$colour <- rgb(0, 0, 0, lineAlpha)

# Combine to 1 panel
panel_C <- plot_grid(recallPlot3, euclidPlot3, AFCPlot3, ncol = 3)

# Combine to 1 figure
figure1 <- plot_grid(panel_A, panel_B, panel_C, nrow = 3, labels = 'AUTO')

save_plot("figure1.png", figure1,
          base_height = 19/cm(1),
          base_width = 19/cm(1),
          base_aspect_ratio = 1)

# /* 
# ----------------------------- Two lines: recall ---------------------------
# */
# Load data
load("U:/Projects/schemaVR/schemaVR_paper/scriptsOutput/twoLineTests_20191022_122817.RData")

# Select right model at breaking point -.2
br1              <- -0.2
recall_twolines  <- results_recall[[7]]

# Create interrupted regression predictors
x <- combinedData_recall$sExp 
combinedData_recall$xlow  <- ifelse(x <= br1, x - br1, 0)
combinedData_recall$xhigh <- ifelse(x > br1, x - br1, 0)     
combinedData_recall$high  <- ifelse(x > br1, 1, 0)

# Extract mean fit
recall_twolines_fixef <- fixef(recall_twolines)
predicted <- recall_twolines_fixef[1, 1] + 
             recall_twolines_fixef[2, 1]*combinedData_recall$xlow +
             recall_twolines_fixef[3, 1]*combinedData_recall$xhigh +
             recall_twolines_fixef[4, 1]*combinedData_recall$high
combinedData_recall$predicted    <- predicted
combinedData_recall$predicted_pr <- logit2prob(predicted)

# Extracting posterior distributions
recall_twolines_postDist <- posterior_samples(recall_twolines)
nDraws <- dim(recall_twolines_postDist)[1]

# Points to plot that are necessary to plot the two lines
minPoint     <- which(combinedData_recall$sExp == min(combinedData_recall$sExp))[1]
middlePoint1 <- which(combinedData_recall$sExp == max(combinedData_recall$sExp[combinedData_recall$sExp < br1]))[1]
maxPoint     <- which(combinedData_recall$sExp == max(combinedData_recall$sExp))[1]
middlePoint2 <- which(round(combinedData_recall$sExp, 2) == br1)[1]
combinedData_recall_sub <- combinedData_recall[c(minPoint, middlePoint1, middlePoint2, maxPoint), ]

# Extracting the fit for all samples
numDataPoints <- dim(combinedData_recall_sub)[1]
drawnFit      <- matrix(rep(NA, nDraws*numDataPoints), nrow = nDraws) 
for(i in 1:nDraws){
  drawnFit[i, ] <- recall_twolines_postDist[i, 1] + 
    recall_twolines_postDist[i, 2]*combinedData_recall_sub$xlow +
    recall_twolines_postDist[i, 3]*combinedData_recall_sub$xhigh +
    recall_twolines_postDist[i, 4]*combinedData_recall_sub$high
}

# Binding to data frame
drawnFit_DF     <- data.frame(id    = c(rep(1:nDraws, 2), rep(1:nDraws + nDraws, 2)), 
                              x     = rep(combinedData_recall_sub$sExp, each = nDraws), 
                              high  = rep(combinedData_recall_sub$high, each = nDraws),
                              logitFit= c(drawnFit))
drawnFit_DF$fit <- logit2prob(drawnFit_DF$logitFit)

# Make factor for colouring
combinedData_recall$Line <- factor(combinedData_recall$high, c(1, 0), labels = c('high', 'low'))
drawnFit_DF$Line         <- factor(drawnFit_DF$high, c(1, 0), labels = c('high', 'low'))

# Plot
recallPlot_twoline <- ggplot(combinedData_recall, aes(x = sExp, y = predicted_pr)) + 
  geom_line(aes(group = high, colour = Line)) + 
  geom_vline(xintercept = br1, linetype = "dashed") +
  geom_jitter(aes(y = accRecall), height = jitterHeight, alpha = pointAlpha, size = pointSize) +
  geom_line(data = drawnFit_DF, aes(x = x, y = fit, group = id, colour = Line), alpha = lineAlpha2, size = lineSize) +
  scale_color_mrc() + 
  labs(y = 'Pr (Recall accuracy)', x = "Expectancy") +
  theme(legend.position = 'none')

# /* 
# ----------------------------- Two lines: 3AFC ---------------------------
# */
# Select right model at breaking point -.35
br2              <- -0.35
AFC_twolines  <- results_AFC[[which(br_AFC == br2)]]

# Create interrupted regression predictors
x <- combinedData_AFC$sExp 
combinedData_AFC$xlow  <- ifelse(x <= br2, x - br2, 0)
combinedData_AFC$xhigh <- ifelse(x > br2, x - br2, 0)     
combinedData_AFC$high  <- ifelse(x > br2, 1, 0)

# Extract mean fit
AFC_twolines_fixef <- fixef(AFC_twolines)
predicted <- AFC_twolines_fixef[1, 1] + 
  AFC_twolines_fixef[2, 1]*combinedData_AFC$xlow +
  AFC_twolines_fixef[3, 1]*combinedData_AFC$xhigh +
  AFC_twolines_fixef[4, 1]*combinedData_AFC$high
combinedData_AFC$predicted    <- predicted
combinedData_AFC$predicted_pr <- logit2prob(predicted)

# Extracting posterior distributions
AFC_twolines_postDist <- posterior_samples(AFC_twolines)
nDraws <- dim(AFC_twolines_postDist)[1]

# Points to plot that are necessary to plot the two lines
minPoint     <- which(combinedData_AFC$sExp == min(combinedData_AFC$sExp))[1]
middlePoint1 <- which(combinedData_AFC$sExp == max(combinedData_AFC$sExp[combinedData_AFC$sExp < br2]))[1]
maxPoint     <- which(combinedData_AFC$sExp == max(combinedData_AFC$sExp))[1]
middlePoint2 <- which(round(combinedData_AFC$sExp, 2) == br2)[1]
combinedData_AFC_sub <- combinedData_AFC[c(minPoint, middlePoint1, middlePoint2, maxPoint), ]

# Extracting the fit for all samples
numDataPoints <- dim(combinedData_AFC_sub)[1]
drawnFit      <- matrix(rep(NA, nDraws*numDataPoints), nrow = nDraws) 
for(i in 1:nDraws){
  drawnFit[i, ] <- AFC_twolines_postDist[i, 1] + 
    AFC_twolines_postDist[i, 2]*combinedData_AFC_sub$xlow +
    AFC_twolines_postDist[i, 3]*combinedData_AFC_sub$xhigh +
    AFC_twolines_postDist[i, 4]*combinedData_AFC_sub$high
}

# Binding to data frame
drawnFit_DF     <- data.frame(id    = c(rep(1:nDraws, 2), rep(1:nDraws + nDraws, 2)), 
                              x     = rep(combinedData_AFC_sub$sExp, each = nDraws), 
                              high  = rep(combinedData_AFC_sub$high, each = nDraws),
                              logitFit= c(drawnFit))
drawnFit_DF$fit <- logit2prob(drawnFit_DF$logitFit)

# Make factor for colouring
combinedData_AFC$Line <- factor(combinedData_AFC$high, c(1, 0), labels = c('high', 'low'))
drawnFit_DF$Line         <- factor(drawnFit_DF$high, c(1, 0), labels = c('high', 'low'))

# Plot
AFCPlot_twoline <- ggplot(combinedData_AFC, aes(x = sExp, y = predicted_pr)) + 
  geom_line(aes(group = high, colour = Line)) + 
  geom_vline(xintercept = br2, linetype = "dashed") +
  geom_jitter(aes(y = accAFC), height = jitterHeight, alpha = pointAlpha, size = pointSize) +
  geom_line(data = drawnFit_DF, aes(x = x, y = fit, group = id, colour = Line), alpha = lineAlpha2, size = lineSize) +
  scale_color_mrc() + 
  labs(y = 'Pr (3AFC accuracy)', x = "Expectancy") + 
  theme(legend.position =c(-0.18, -0.1), 
        legend.title=element_blank(),
        legend.box = "horizontal",
        legend.key = element_rect(colour = NA, fill = NA),
        legend.key.size = unit(0.3, "cm"),
        legend.box.spacing = unit(0.01, "cm"),
        legend.background = element_rect(fill= NA))


# Combine to 1 panel
panel_E <- plot_grid(recallPlot_twoline, 
                     AFCPlot_twoline, 
                     ncol = 2,
                     align = 'h',
                     axis = "tblr")

save_plot("panel_E.png", panel_E,
          base_height = 6/cm(1),
          base_width = 12/cm(1),
          base_aspect_ratio = 1)


# /* 
# ----------------------------- Spread across participants ---------------------------
# */
# Loading data
load("U:/Projects/schemaVR/schemaVR1/data/dataSchemaVR1_cleaned.RData")
load("U:/Projects/schemaVR/schemaVR2/data/dataSchemaVR2_cleaned.RData")
load("U:/Projects/schemaVR/schemaVR3/data/dataSchemaVR3_cleaned.RData")

# Combining data sets 
combinedRatings <- data.frame(Participant = c(as.character(dataSchemaVR1$subNum),
                                              as.character(dataSchemaVR2$subNum),
                                              as.character(dataSchemaVR3$subNum)),
                              Expectancy = c(dataSchemaVR1$objLocTargetRating,
                                             dataSchemaVR2$objLocTargetRating,
                                             dataSchemaVR3$objLocTargetRating),
                              Experiment = c(rep('1', length(dataSchemaVR1$subNum)),
                                             rep('2', length(dataSchemaVR2$subNum)),
                                             rep('3', length(dataSchemaVR3$subNum))))

ratingsSpread <- ggplot(combinedRatings, aes(x = Expectancy, y = Participant)) +
  geom_line(aes(colour = Experiment)) +  
  geom_point(aes(colour = Experiment)) +  
  scale_color_mrc() +
  scale_y_discrete(limits = combinedRatings$Participant, expand = expand_scale(0.01)) +
  labs(title = 'Spread in expectancy ratings for each participant') +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank())

ratingHist <- ggplot(combinedRatings, aes(x = Expectancy)) + 
  geom_histogram() +
  labs(title = 'Histogram of expectancy ratings across participants', y = 'Count') +
  theme(axis.title.x = element_blank())

ratingsPlots <- plot_grid(ratingHist, 
                          ratingsSpread,
                          nrow = 2,
                          align = 'v',
                          axis = "tblr",
                          rel_heights = c(1, 2))

save_plot("ratingsPlots.png", ratingsPlots,
          base_height = 16/cm(1),
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