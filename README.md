GitHub Repository for the __schemaVR__ project
================

Collaborators: Joern Alexander Quent, Andrea Greve & Rik Henson
Status: __In Preparation__ 

The main aim of this project was to provide evidence that memory and expectancy follow a U-shape as predicted by the SLIMM framework. 

For more information see pre-print: https://psyarxiv.com/xq37j/

This repository contains all the relevant code and material for this project.

# How to use this repository: 
1. Clone the repository. 
2. Run the script (***downloads_and_moves_files_from_osf.R***) that will download all the _.RData_ files that are too large for GitHub and move them to the correct locations. 
3. See _Guide_ how to navigate the repository. 
4. If there are questions, please contact me _alexander.quent AT rub.de_.

# Guide through repository
## Files

- ***downloads_and_moves_files_from_osf.R*** Script that downloads files that are too large to be uploaded to GitHub and moves content to correct file locations. These files are mainly the fitted models that are generated in the schemaVR folders and important to run the result files. The required disk space for this is approximately 11 GB. 
- ***paper.Rproj*** file (R project file) can be found in the paper folder. 

## Folders and their content

Note that these folders only exist in the local copy that includes raw data:

- ***normativeStudy*** contains the data from the normative study. For more information and code see https://github.com/JAQuent/ratingStudy. 
- ***paper*** contains result documents, material for figures and some background work.  
- ***schemaVR1*** to ***schemaVR4***  contain all material that is specific to the various iterations of the experiment. Most importantly these folders contain the analysis scripts under `\analysis\scripts`

# More information

The build task can be found [here](https://github.com/JAQuent/schemaVR/blob/master/schemaVR3/experiment/build_schemaVR3_task.zip). The unity project for schemaVR3 can be found [here](https://osf.io/ekzj9/download). 