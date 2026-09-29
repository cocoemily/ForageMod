#!/bin/bash

#SBATCH --job-name=process-vt
#SBATCH --output=sbatch-files/process-vt_%j.txt
#SBATCH --time=24:00:00
#SBATCH --mail-type=ALL
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=1
#SBATCH --mem-per-cpu=250G

module purge
module load R/4.3.2-foss-2022b
module load UDUNITS/2.2.28-GCCcore-12.2.0
module load GDAL/3.6.2-foss-2022b

#Rscript hpc_scripts/veg-morans-i-and-diversity_prod100.R
#Rscript hpc_scripts/veg-morans-i-and-diversity_unprod100.R
#Rscript hpc_scripts/veg-morans-i-and-diversity_prod250.R
Rscript hpc_scripts/veg-morans-i-and-diversity_unprod250.R
