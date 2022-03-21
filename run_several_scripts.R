# This script runs several scripts in succession
# Version 1.0
# /* 
# ----------------------------- Input ---------------------------
# */
######################################################
# Path to parent folder 
path2folder <- "E:/Alex/Laptop/Desktop/schemaVR/" # This need to be changed to run this document
######################################################

# Lib
library(assortedRFunctions)

# How many minutes should be waited be between scrips
wait_time_minutes <- 1

# Add all script names that are in the same folder
# Note the order of the scripts might be important if they depend on each other. 
script_names  <- c(#'schemaVR1/analysis/scripts/schemaVR1_AFC.R',
                   #'schemaVR2/analysis/scripts/schemaVR2_AFC_zeroPrior.R',
                   #'schemaVR3/analysis/scripts/schemaVR3_AFC_zeroPrior.R',
                   #'schemaVR4/analysis/scripts/schemaVR4_AFC_zeroPrior.R',
                   #'schemaVR2/analysis/scripts/schemaVR2_AFC_sequential.R',
                   #'schemaVR3/analysis/scripts/schemaVR3_AFC_sequential.R',
                   #'schemaVR4/analysis/scripts/schemaVR4_AFC_sequential.R',
                   #'schemaVR1/analysis/scripts/schemaVR1_recall.R',
                   #'schemaVR2/analysis/scripts/schemaVR2_recall_zeroPrior.R',
                   #'schemaVR3/analysis/scripts/schemaVR3_recall_zeroPrior.R',
                   #'schemaVR4/analysis/scripts/schemaVR4_recall_zeroPrior.R',
                   #'schemaVR2/analysis/scripts/schemaVR2_recall_sequential.R',
                   #'schemaVR3/analysis/scripts/schemaVR3_recall_sequential.R',
                   #'schemaVR4/analysis/scripts/schemaVR4_recall_sequential.R',
                   #'schemaVR2/analysis/scripts/schemaVR1_2_recall_zeroPrior_interaction.R',
                   #'schemaVR2/analysis/scripts/schemaVR2_RK_zeroPrior.R',
                   #'schemaVR3/analysis/scripts/schemaVR3_RK_zeroPrior.R',
                   #'schemaVR4/analysis/scripts/schemaVR4_RK_zeroPrior.R',
                   #'schemaVR3/analysis/scripts/schemaVR3_RK_sequential.R',
                   'schemaVR4/analysis/scripts/schemaVR4_RK_sequential.R'
                   )

# How do you want to run the scripts source (data can be used): run_type <- 1
# or system (data can't be used between scripts): run_type <- 2
run_type <- 2

# If you use run_type, then you need to specify the path to R.exe
path2R <- "C:/Program Files/R/R-4.1.0/bin/Rscript.exe"

# /* 
# ----------------------------- Preparation ---------------------------
# */
# path2scripts and number of scripts
paths2scripts <- paste0(path2folder, script_names)
n             <- length(script_names) # number of scripts to run

# Only for system (i.e. run_type == 2)
if(run_type == 2){
  # Change from / to  \\
  path2R         <- gsub("/", "\\\\", path2R)
  paths2scripts  <- gsub("/", "\\\\", paths2scripts)
}

# /* 
# ----------------------------- Loop through the scripts ---------------------------
# */
startTime <- Sys.time()

# Loop
if(run_type == 1){
  # Use source
  for(i in 1:n){
    # Run
    source(paths2scripts[i])
    
    # Sleep
    Sys.sleep(wait_time_minutes*60)
    
    # Display progress bar
    progressBar_plot(i, n)
  }
  
} else if(run_type == 2){
  # Use system
  for(i in 1:n){
    # Run
    system("cmd.exe", input = paste0('"', path2R, '" ', paths2scripts[i]))
    
    # Sleep
    Sys.sleep(wait_time_minutes*60)
    
    # Display progress bar
    progressBar_plot(i, n)
  }
} else {
  stop('Wrong run_type input')
}


endTime <- Sys.time()

# How long did it take?
endTime - startTime