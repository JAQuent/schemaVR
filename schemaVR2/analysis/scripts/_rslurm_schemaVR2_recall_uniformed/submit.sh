#!/bin/bash
#
#SBATCH --array=0-0
#SBATCH --job-name=schemaVR2_recall_uniformed
#SBATCH --output=slurm_%a.out
/usr/lib64/R/bin/Rscript --vanilla slurm_run.R
