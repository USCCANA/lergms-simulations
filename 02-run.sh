#!/bin/sh
#SBATCH --job-name="02-various-sizes-3-5"
#SBATCH --output="logs/02-various-sizes-3-5-slurm%A.out"
#SBATCH --time="12:00:00"
#SBATCH --mem-per-cpu=16G
#SBATCH --partition=conti
#SBATCH --account=lc_dvc
#SBATCH --mail-type=ALL
/usr/usc/R/3.5.0/lib64/R/bin/Rscript --vanilla \
	simulations/02-various-sizes-3-5.R
