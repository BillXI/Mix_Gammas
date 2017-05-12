#!/bin/bash
#SBATCH --nodes=1 --ntasks-per-node=1 --mem=2G 
#SBATCH -t 0-00:05
#SBATCH --mail-user=user@uky.edu
#SBATCH --mail-type=ALL

# change dir to your working dir

cd /home/xch234/mix_gammas/Mix_Gammas/Code/2017_5_11
Rscript Condition_13_500.R
