# Script to download and move .RData files to correct locations because they are too large for GitHub
# Version 1.0
# Date:  23/09/2021
# Author: Joern Alexander Quent
# /* 
# ----------------------------- How to use ---------------------------
# */
# Caution: This script will take time to complete!

# If everything works correctly, this script will download all .zip archives (nearly 20 GB in total) from OSF that contain the 
# .RData files that too large to be uploaded to GitHub and automatically moves them to the right locations
# in the schemaVR folder that you get by cloning https://github.com/JAQuent/schemaVR. 

# In order to use script, you merely need to set the variable path2parent (see below) to the location where 
# ever you cloned the repository to. 

# If for any reason, R won't download the .zip files, jsut navigate to https://osf.io/4sw2t/ and download
# RData_files_from_osf1.zip to RData_files_from_osf5.zip manually directly into the schemaVR folder. Downloading
# via R might fail because you don't use windows, for which this script is written. The script will check if all
# archives again and download all of them if not. 

# If that script doesn't run at all, the correct file locations are found in fileLocations.txt. 

# Get start time
startTime <- Sys.time()

######################################################
# Path to parent folder schemaVR that you get from GitHub
path2parent <- "D:/Alex/Laptop/Desktop/schemaVR" # This need to be changed to run this document
######################################################

# download the files from those OSF urls
urls <- c("https://osf.io/f8wsd/download",  
          "https://osf.io/wvd2j/download",
          "https://osf.io/tk3jg/download",
          "https://osf.io/jstv4/download",
          "https://osf.io/txwjs/download")

# Internal variable/file names etc.
fileNames  <- c("RData_files_from_osf1.zip", 
                "RData_files_from_osf2.zip", 
                "RData_files_from_osf3.zip", 
                "RData_files_from_osf4.zip", 
                "RData_files_from_osf5.zip")
tempStore  <- "temporayStorage"

# /* 
# ----------------------------- Preparation  ---------------------------
# */
## Pre-Preparation including lib loading/installing
# Check if the lib exist and if not install
if("filesstrings" %in% rownames(installed.packages())){
  library(filesstrings)
} else {
  install.packages('filesstrings')
  library(filesstrings)
}

## Preparation
# Setting WD
setwd(path2parent)

# /* 
# ----------------------------- Check if archive are already downloaded manually  ---------------------------
# */
files_present <- list.files()
downloaded    <- all(fileNames %in% files_present)

# /* 
# ----------------------------- Download ---------------------------
# */
## Only download again if at least one archive is not already in the folder
if(!downloaded){
  # Download the file from OSF via loop
  for(i in 1:length(urls)){
    download.file(urls[i],  fileNames[i], mode = 'wb', cacheOK=FALSE) # mode = 'wb' is for Windows
  }  
}

# /* 
# ----------------------------- Unzip and move to correct folder---------------------------
# */
# Create temporary directory for copying all files into it
dir.create(tempStore)

# Unzip the file to tempStore via a loop
for(i in 1:lengths(urls)){
  unzip(fileNames[i], exdir = paste0(path2parent, "/", tempStore)) 
}

# Load .txt file with file names and location for moving
fileLocations <- read.table(paste0(tempStore, "/fileLocations.txt"), sep = "\t", header = TRUE)

## Move from tempStore to destinations
move_files(paste0(path2parent, "/", tempStore, "/", fileLocations$fileName), 
           paste0(path2parent, fileLocations$destination), 
           overwrite = FALSE)

# /* 
# ----------------------------- Clean-up steps ---------------------------
# */
## Post steps
# Remove temporary directory
dir.remove(tempStore)

# Get end time
endTime <- Sys.time()

elapsedTime <- round(as.numeric(difftime(endTime, startTime, units = 'mins')), 1)

# Print message at the end
cat(" ########################################\n",
    paste0('File moving finished after ', elapsedTime, ' minutes. \n'),
    'If there were no error messages, you can now delete the .zip files. \n', 
    "########################################\n")