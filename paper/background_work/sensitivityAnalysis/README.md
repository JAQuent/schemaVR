README
================

This README provide a quick overview of what is to be found in this folder. 

# Folders and their content
- ***ignore_workspaces*** contains all images that have been saved and cane be used for analysis. Since these files are large, they are not included in this repository. They can be created by running get_rslurm_results.R. List of all files in this folder:
	- ***freq1.RData*** Logistic frequentist simulation to provide comparison with Bayes Factors.
	- ***log1_BF.RData*** Preliminary simulation 1 for different DF (Scale = 2.5).
	- ***log2_BF.RData*** Preliminary simulation 2 for different DF (Scale = 1).
	- ***log3_BF.RData*** Full simulation for normal & student priors with scale from 1 to 2.5 for different beta values.
	- ***log3_extra_BF.RData*** Full simulation for normal & student priors with scale of 0.5 for different beta values.
	- ***logisticExample.RData*** Example to illustrate effect of different scales on Bayes Factor.
	- ***nullData.RData*** Using previous data to generated null data (for p-values) by shuffling.
	- ***nulldata_BF.RData*** Generated null data fo Bayesian interrupted regression.
	- ***nullData_input.RData*** This data is used as input for the shuffling script.
	- ***sensitivity_analysis_2options_beta1_equals_0_2.RData*** Simulation of frequentist logistic model with beta1 = 0 and different beta2s. 
	- ***sensitivity_analysis_option2_different_beta_BF.RData*** Simulation of Bayesian logistic model with beta1 = 0 and different beta2s. 
- ***rslurmFolders*** contains all rslurm folders that are important to create the images above. List of all files and folders:
	- ***\_rslurm_logistic_regression***  Preliminary simulation 1 for different DF (Scale = 2.5)
	- ***\_rslurm_logistic_regression_freq*** Logistic frequentist simulation to provide comparison with Bayes Factors
	- ***\_rslurm_logistic_regression2*** Preliminary simulation 2 for different DF (Scale = 1)
	- ***\_rslurm_logistic_regression3*** Full simulation for normal & student priors with scale from 1 to 2.5 for different beta values
	- ***\_rslurm_logistic_regression3_extra*** Full simulation for normal & student priors with scale of 0.5 for different beta values
	- ***\_rslurm_nullData_BF*** Generated null data fo Bayesian interrupted regression
	- ***\_rslurm_option1*** Simulation of frequentist logistic model with beta1 = 0 and different beta2s (option 1). 
	- ***\_rslurm_option2*** Simulation of frequentist logistic model with beta1 = 0 and different beta2s (option 2). 
	- ***\_rslurm_simulated_BF*** Simulation of Bayesian logistic model with beta1 = 0 and different beta2s. 

# Scripts 
- ***get_rslurm_results.R*** This script loads rslurm folders and creates images that can be load with R. 
- ***estimatingCovariance.R*** This script is an attempt to estimate the covariance between bins of expectancy to be used in simulation null data that accounts for this dependency. However, we found in the original data there is little correlation between bins and therefore we abandoned this approach.  
- ***nullData_for_correction.R*** This scripts create null data by shuffling the existing data.
- ***nullData_for_correction_rslurm.R*** This scripts create null data by shuffling the existing data (rslurm version).
- ***prior_simulation_logistic_BF.R***  Script for simulation for priors in logistic regression
- ***sensitivity_analysis_2options.R*** Script for simulation of frequentist logistic model with beta1 = 0 and different beta2s. 
- ***sensitivity_analysis_2options_rslurm.R*** Script for simulation of frequentist logistic model with beta1 = 0 and different beta2s (rslurm version). 
- ***simulation_logistic_freq.R*** Script for logistic frequentist simulation to provide comparison with Bayes Factors.

# (R)Markdown files
- ***prior_simulation_logistic.md***
- ***prior_simulation_logistic.Rmd***
- ***sensitivity_analysis_2options_notes.md***
- ***sensitivity_analysis_2options_notes.Rmd***

# .RData files
- ***nullData_input.RData*** This data is used as input for the shuffling script