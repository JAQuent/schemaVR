#!/bin/bash
#
#SBATCH --array=0-19
#SBATCH --cpus-per-task=22
#SBATCH --job-name=logistic_regression_freq
#SBATCH --output=slurm_%a.out
#SBATCH --time=2-12
/imaging/local/software/R/3.5.3shlib/bin/Rscript --vanilla slurm_run.R
