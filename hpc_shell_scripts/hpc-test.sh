#!/bin/bash

#SBATCH --job-name=hpc-test
#SBATCH --output=sbatch-files/hpc-test_%j.txt
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=1
#SBATCH --mem-per-cpu=300
#SBATCH --time=1:00:00
#SBATCH --mail-user=emily.coco@yale.edu

module purge
module load Java/17.0.4
module load NetLogo/6.4.0-64

#PATH=$PATH:/home/ec2295/project/NetLogo
EXP_NAME="HPC-test"
echo $EXP_NAME

export _JAVA_OPTIONS='-Dcom.sun.media.jai.disableMediaLib=true -Xmx5120m -Dfile.encoding=UTF-8'

bash netlogo-headless.sh --model "ForageModv02.nlogo" --experiment "HPC-test" --update-plots --threads 1

#./NetLogo_Console --headless --model "/home/ec2295/project/ForageMod/ForageModv02.nlogo" --experiment "HPC-test" --update-plots
