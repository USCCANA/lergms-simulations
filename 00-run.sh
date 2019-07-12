#!/bin/sh
#SBATCH --job-name="00-dgp"
#SBATCH --output="logs/00-dgp%A.out"
#SBATCH --time="24:00:00"
#SBATCH --mem-per-cpu=8G
#SBATCH --partition=conti
#SBATCH --account=lc_dvc
#SBATCH --mail-type=ALL
/usr/usc/R/3.5.0/lib64/R/bin/Rscript --vanilla \
	simulations/dgp.R
