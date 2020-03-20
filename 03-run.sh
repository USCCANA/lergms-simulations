#!/bin/sh
#SBATCH --job-name="03-various-sizes-4-5-main"
#SBATCH --output="logs/03-various-sizes-4-5-main.out"
#SBATCH --time="12:00:00"
#SBATCH --mem-per-cpu=16G
#SBATCH --partition=scavenge
# #SBATCH --account=lc_dvc
#SBATCH --mail-type=ALL
/usr/usc/R/3.5.0/lib64/R/bin/Rscript --vanilla \
	simulations/03-various-sizes-4-5.R
