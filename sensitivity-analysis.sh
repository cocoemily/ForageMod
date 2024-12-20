#!/bin/bash

#SBATCH --job-name=sanalysis
#SBATCH --output=sbatch-files/sanalysis_%j.txt
#SBATCH --partition week
#SBATCH --time=48:00:00
#SBATCH --cpus-per-task=32
#SBATCH --mem-per-cpu=8G
#SBATCH --mail-type=ALL
#SBATCH --mail-user=emily.coco@yale.edu

module purge
module load Java/17.0.4
module load NetLogo/6.4.0-64

export _JAVA_OPTIONS='-Dcom.sun.media.jai.disableMediaLib=true -Xmx8192m -Dfile.encoding=UTF-8'

echo $SLURM_NTASKS
#bash netlogo-headless.sh --model "ForageModv02.nlogo" --experiment "sensitivity_analysis_high-med-low" --update-plots --threads 32
bash netlogo-headless.sh --model "ForageModv02.nlogo" --experiment "sensitivity-analysis_time" --update-plots --threads 32
