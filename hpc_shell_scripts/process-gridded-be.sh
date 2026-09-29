#!/bin/bash

#SBATCH --job-name=process-be
#SBATCH --output=sbatch-files/process-be_%j.txt
#SBATCH --time=24:00:00
#SBATCH --mail-type=ALL
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=1
#SBATCH --mem-per-cpu=250G

module purge
#module load R/4.3.2-foss-2022b
module load R-bundle-CRAN/2023.12-foss-2022b
module load UDUNITS/2.2.28-GCCcore-12.2.0
module load GDAL/3.6.2-foss-2022b
module load CMake/3.24.3-GCCcore-12.2.0

#Rscript hpc_scripts/burning-events-morans-i_cycle100.R
#Rscript hpc_scripts/burning-events-morans-i_cycle250.R
Rscript hpc_scripts/visualize_burning-events.R

