#!/bin/bash
#
#SBATCH --array=0-3
#SBATCH --cpus-per-task=20
#SBATCH --job-name=option1
#SBATCH --output=slurm_%a.out
/imaging/local/software/R/3.5.3shlib/bin/Rscript --vanilla slurm_run.R
