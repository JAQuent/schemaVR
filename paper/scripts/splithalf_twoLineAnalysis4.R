# Script performance a split half two line analysis for recall: Part 4
# Version 1.0
# Date: 14/11/2019
# Author: Joern Alexander Quent
# /*
# ----------------------------- Libraries, settings and functions ---------------------------
# */
# Setting seed and cores
set.seed(9544)

# Libraries
library(brms)
library(rslurm)

# /*
# ----------------------------- Load results and combine models ---------------------------
# */

results <- readRDS('_rslurm_splithalf_recall2/results_0.RDS', refhook = NULL)
m_all <- combine_models(results[[1]]$model,
                        results[[2]]$model,
                        results[[3]]$model,
                        results[[4]]$model,
                        results[[5]]$model,
                        results[[6]]$model,
                        results[[7]]$model,
                        results[[8]]$model)