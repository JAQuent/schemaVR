# Library
library(assortedRFunctions)

# Set WD
setwd("C:/Users/aq01/Desktop/schemaVR/paper/scripts/Sensitivity analysis")

# /*
# ----------------------------- Frequentist simulation --------------------------
# */
# Read in and rslurm results for sensitivity analysis
sim_df_option1 <- get_rslurm_results('C:/Users/aq01/Desktop/schemaVR/paper/scripts/Sensitivity analysis/_rslurm_option1/')
sim_df_option2 <- get_rslurm_results('C:/Users/aq01/Desktop/schemaVR/paper/scripts/Sensitivity analysis/_rslurm_option2/')


# Save image
save.image('sensitivity_analysis_2options_generated.RData')

# /*
# ----------------------------- Shuffled data with sub and obj as random intercept (nullData) --------------------------
# */
# Remove everything
rm(list=ls()) 

sim_df <- get_rslurm_results('C:/Users/aq01/Desktop/schemaVR/paper/scripts/Sensitivity analysis/_rslurm_nullData_sim/')

# Save image 
save.image('sensitivity_analysis_shuffled.RData')


# ----------------------------- BF null data (with 0.5 scaling) --------------------------
# */
# Remove everything
rm(list=ls()) 

nulldata_BF_scaling_0_5 <- get_rslurm_results("C:/Users/aq01/Desktop/schemaVR/paper/scripts/Sensitivity analysis/_rslurm_nullData_BF_3_scaling_0_5/")

# Save 
save.image('nullData_BF_3_scaling_0_5.RData')


# /*
# ----------------------------- BF with different beta (0.5 scaling) --------------------------
# */
# Remove everything
rm(list=ls()) 

option2_BF <-  get_rslurm_results('C:/Users/aq01/Desktop/schemaVR/paper/scripts/Sensitivity analysis/_rslurm_simulated_BF/')

# Save 
save.image('sensitivity_analysis_option2_different_beta_BF.RData')

# /*
# ----------------------------- Logistic simulation 1 --------------------------
# */
# Remove everything
rm(list=ls()) 

log1_BF <-  get_rslurm_results('C:/Users/aq01/Desktop/schemaVR/paper/scripts/Sensitivity analysis/_rslurm_logistic_regression/')

# Save 
save.image('log1_BF.RData')

# /*
# ----------------------------- Logistic simulation 2 --------------------------
# */
# Remove everything
rm(list=ls()) 

log2_BF <-  get_rslurm_results('C:/Users/aq01/Desktop/schemaVR/paper/scripts/Sensitivity analysis/_rslurm_logistic_regression2/')

# Save 
save.image('log2_BF.RData')


# /*
# ----------------------------- Logistic simulation 3 --------------------------
# */
# Remove everything
rm(list=ls()) 

log3_BF <-  get_rslurm_results('C:/Users/aq01/Desktop/schemaVR/paper/scripts/Sensitivity analysis/_rslurm_logistic_regression3/')

# Save 
save.image('log3_BF.RData')


# /*
# ----------------------------- Logistic simulation extra--------------------------
# */
# Remove everything
rm(list=ls()) 

log3_extra_BF <-  get_rslurm_results('C:/Users/aq01/Desktop/schemaVR/paper/scripts/Sensitivity analysis/_rslurm_logistic_regression3_extra/')

# Save 
save.image('log3_extra_BF.RData')



# /*
# ----------------------------- Logistic freq simulation --------------------------
# */
# Remove everything
rm(list=ls()) 

freq1 <-  get_rslurm_results('C:/Users/aq01/Desktop/schemaVR/paper/scripts/Sensitivity analysis/_rslurm_logistic_regression_freq/')

# Save 
save.image('freq1.RData')
