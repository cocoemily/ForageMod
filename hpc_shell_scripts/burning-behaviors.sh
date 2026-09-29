#!/bin/bash

#SBATCH --job-name=bb-exp
#SBATCH --output=sbatch-files/bb_%j.txt
#SBATCH --partition day
#SBATCH --time=24:00:00
#SBATCH --cpus-per-task=32
#SBATCH --mem-per-cpu=4G
#SBATCH --mail-type=ALL
#SBATCH --mail-user=emily.coco@yale.edu

module purge
module load Java/17.0.4
module load NetLogo/6.4.0-64

export _JAVA_OPTIONS='-Dcom.sun.media.jai.disableMediaLib=true -Xmx5120m -Dfile.encoding=UTF-8'

echo $SLURM_NTASKS
bash netlogo-headless.sh --model "ForageModv02.nlogo" --experiment "burning-behaviors" --update-plots --threads 32
