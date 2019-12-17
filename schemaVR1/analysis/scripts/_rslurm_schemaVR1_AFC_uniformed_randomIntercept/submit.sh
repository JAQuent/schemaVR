#!/bin/bash
#
#SBATCH --array=0-0
#SBATCH --job-name=schemaVR1_AFC_uniformed_randomIntercept
#SBATCH --output=slurm_%a.out
/usr/lib64/R/bin/Rscript --vanilla slurm_run.R
