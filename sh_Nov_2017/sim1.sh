#!/bin/bash
#SBATCH --nodes=1 --ntasks-per-node=1 --mem=2G 
#SBATCH -t 6-00:00   
#SBATCH --mail-user=xch234@uky.edu
#SBATCH --mail-type=ALL

# change dir to your working dir

cd /home/xch234/mix_gammas/Mix_Gammas/Code/2017_11_24
Rscript Condition_12_500.R
