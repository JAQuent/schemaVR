#!/bin/bash
#
#SBATCH --array=0-3
#SBATCH --job-name=twoline_sim3
#SBATCH --output=slurm_%a.out
/usr/lib64/R/bin/Rscript --vanilla slurm_run.R
