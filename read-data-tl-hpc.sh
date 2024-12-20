#!/bin/bash

#SBATCH --job-name=tl-data
#SBATCH --output=sbatch-files/read-data-tl_%j.txt
#SBATCH --time=5:00:00
#SBATCH --mail-type=ALL
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=1
#SBATCH --mem-per-cpu=300G

module purge
module load R/4.3.2-foss-2022b

Rscript hpc_scripts/read-data_pop-locations.R
