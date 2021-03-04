Readme for this folder
================

# Guide through this folder
## Folders and their content
- *ignore_workspaces* contains all images that have been saved and cane be used for analysis. Since these files are large, they are not included in this repository. They can be created by running get_rslurm_results.R. List of all files and folders:
	- freq1.RData
	- log1_BF.RData
	- log2_BF.RData
	- log3_BF.RData
	- log3_extra_BF.RData
	- logisticExample.RData
	- nullData.RData
	- nulldata_BF.RData
	- nullData_BF_3_scaling_0_5.RData
	- nullData_input.RData
	- nullData_sub_obj.RData
	- option_BF_df.RData
	- sensitivity_analysis_2options.RData
	- sensitivity_analysis_2options_beta1_equals_0_1.RData
	- sensitivity_analysis_2options_beta1_equals_0_2.RData
	- sensitivity_analysis_option2_different_beta_BF.RData
	- sensitivity_analysis_option2_generated_BF.RData


## Scripts 
get_rslurm_results.R

estimatingCovariance.R: This script is an attempt to estimate the covariance between bins of expectancy to be used in simulation null data that accounts for this dependency. However, we found in the original data there is little correlation between bins and therefore we abandoned this approach.  

