# Script to combine all model for schemaVR1
# Version 1.0
# Date:  19/11/2019
# Author: Joern Alexander Quent
# /* 
# ----------------------------- Libraries, settings and functions ---------------------------
# */
# Clear workspace
rm(list = ls())

# Libraries
library(brms)
library(rslurm)

# List of all parent folders
pathParentFolder <- c('U:/Projects/schemaVR/schemaVR1/analysis/scripts',
                      'U:/Projects/schemaVR/schemaVR2/analysis/scripts',
                      'U:/Projects/schemaVR/schemaVR3/analysis/scripts')

pattern <- '_rslurm_'
# /* 
# ----------------------------- Going through all folders ---------------------------
# */
for(i in 1:length(pathParentFolder)){
  # Get all rslurm folders in that parent folder
  allFiles      <- list.files(pathParentFolder[i])
  rslurmFolders <- allFiles[grepl(pattern, allFiles)]
  modelNames    <- gsub(paste0(pattern, '(.+)'), '\\1', rslurmFolders)
  
  # Go through all folders in that parent folder
  for(j in 1:length(rslurmFolders)){
    # Read rlsurm file
    tempList <- readRDS(paste0(pathParentFolder[i],
                               '/', 
                               rslurmFolders[j], 
                               '/results_0.RDS'), 
                        refhook = NULL)
    
    # Combine all 8 runs of the model
    tempModel <- combine_models(tempList[[1]]$model,
                                tempList[[2]]$model,
                                tempList[[3]]$model,
                                tempList[[4]]$model,
                                tempList[[5]]$model,
                                tempList[[6]]$model,
                                tempList[[7]]$model,
                                tempList[[8]]$model)
    
    # Assign to model name that is taken from folder name
    assign(modelNames[j], tempModel)
  }
}

# Remove temporary files
rm('tempList')
rm('tempModel')

# /* 
# ----------------------------- Save image ---------------------------
# */
save.image('combinedModels.RData')