#!/bin/bash

#SBATCH --job-name=read-data
#SBATCH --output=sbatch-files/read-data_%j.txt
#SBATCH --time=24:00:00
#SBATCH --mail-type=ALL
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=1
#SBATCH --mem-per-cpu=5G

module purge
module load R/4.3.2-foss-2022b

#Rscript hpc_scripts/sensitivity-analysis-parameters.R
Rscript hpc_scripts/sensitivity-analysis-time.R
