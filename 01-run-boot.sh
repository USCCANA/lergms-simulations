#!/bin/sh
#SBATCH --job-name="01-fixed-sizes-4-boot"
#SBATCH --output="01-fixed-sizes-4-boot-slurm%A.out"
#SBATCH --time="12:00:00"
#SBATCH --mem-per-cpu=8G
#SBATCH --partition=thomas
#SBATCH --account=lc_pdt
#SBATCH --mail-type=ALL
/usr/usc/R/3.4.4/lib64/R/bin/Rscript --vanilla \
	simulations/01-fixed-sizes-4-boot.R
