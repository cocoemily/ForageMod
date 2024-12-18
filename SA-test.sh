#!/bin/bash

#SBATCH --job-name=SA-test
#SBATCH --output=sbatch-files/SA-test_%j.txt
#SBATCH --time=24:00:00
#SBATCH --ntasks=32
#SBATCH --cpus-per-task=1
#SBATCH --mem-per-cpu=4G
#SBATCH --mail-user=emily.coco@yale.edu

module purge
module load Java/17.0.4
module load NetLogo/6.4.0-64

export _JAVA_OPTIONS='-Dcom.sun.media.jai.disableMediaLib=true -Xmx5120m -Dfile.encoding=UTF-8'

echo $SLURM_NTASKS
bash netlogo-headless.sh --model "ForageModv02.nlogo" --experiment "sensitivity-analysis_TEST" --update-plots --threads $SLURM_NTASKS
