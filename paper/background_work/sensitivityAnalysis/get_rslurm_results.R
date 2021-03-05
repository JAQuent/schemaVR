# This script loads rslurm folders and creates images that can be load with R. 
# Library
library(assortedRFunctions)

# Set WD
setwd("paper/background_work/sensitivityAnalysis")

# /*
# ----------------------------- Frequentist simulation --------------------------
# */
# Read in and rslurm results for sensitivity analysis
sim_df_option1 <- get_rslurm_results('background_work/sensitivityAnalysis/rslurmFolders/_rslurm_option1/')
sim_df_option2 <- get_rslurm_results('background_work/sensitivityAnalysis/rslurmFolders/_rslurm_option2/')


# Save image
save.image('ignore_workspaces/sensitivity_analysis_2options_beta1_equals_0_2.RData')


# /*
# ----------------------------- BF with different beta (0.5 scaling) --------------------------
# */
# Remove everything
rm(list=ls()) 

option2_BF <-  get_rslurm_results('background_work/sensitivityAnalysis/rslurmFolders/_rslurm_simulated_BF/')

# Save 
save.image('ignore_workspaces/sensitivity_analysis_option2_different_beta_BF.RData')

# /*
# ----------------------------- Logistic simulation 1 --------------------------
# */
# Remove everything
rm(list=ls()) 

log1_BF <-  get_rslurm_results('background_work/sensitivityAnalysis/rslurmFolders/_rslurm_logistic_regression/')

# Save 
save.image('ignore_workspaces/log1_BF.RData')

# /*
# ----------------------------- Logistic simulation 2 --------------------------
# */
# Remove everything
rm(list=ls()) 

log2_BF <-  get_rslurm_results('background_work/sensitivityAnalysis/rslurmFolders/_rslurm_logistic_regression2/')

# Save 
save.image('ignore_workspaces/log2_BF.RData')


# /*
# ----------------------------- Logistic simulation 3 --------------------------
# */
# Remove everything
rm(list=ls()) 

log3_BF <-  get_rslurm_results('background_work/sensitivityAnalysis/rslurmFolders/_rslurm_logistic_regression3/')

# Save 
save.image('ignore_workspaces/log3_BF.RData')


# /*
# ----------------------------- Logistic simulation extra--------------------------
# */
# Remove everything
rm(list=ls()) 

log3_extra_BF <-  get_rslurm_results('background_work/sensitivityAnalysis/rslurmFolders/_rslurm_logistic_regression3_extra/')

# Save 
save.image('ignore_workspaces/log3_extra_BF.RData')



# /*
# ----------------------------- Logistic freq simulation --------------------------
# */
# Remove everything
rm(list=ls()) 

freq1 <-  get_rslurm_results('background_work/sensitivityAnalysis/rslurmFolders/_rslurm_logistic_regression_freq/')

# Save 
save.image('ignore_workspaces/freq1.RData')
