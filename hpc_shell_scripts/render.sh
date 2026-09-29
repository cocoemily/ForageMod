#!/bin/bash

#SBATCH --job-name=render_markdown
#SBATCH --output=sbatch-files/render-rmd_%j.txt
#SBATCH --time=24:00:00
#SBATCH --mail-type=ALL
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=1
#SBATCH --mem-per-cpu=100G

module purge
#module load R/4.3.2-foss-2022b
module load R-bundle-CRAN/2023.12-foss-2022b
module load UDUNITS/2.2.28-GCCcore-12.2.0
module load GDAL/3.6.2-foss-2022b
module load CMake/3.24.3-GCCcore-12.2.0
module load Pandoc/3.1.2
module load texlive/20220321-GCC-12.2.0

Rscript analysis_scripts/render_markdown.R
#Rscript analysis_scripts/burnt-patch-proportions.R

