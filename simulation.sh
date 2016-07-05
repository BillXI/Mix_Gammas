#!/bin/bash

# The first line indicates the intepretor: bash/Rscript/sh/...



# MAX use of cpys and memory

#PBS -l nodes=2,ncpus=30,mem=4G,walltime=48:00:00

#PBS -q default

#PBS -M s@uky.edu

#PBS -m abe



# change dir to your working dir

cd /home/xch234/mix_gammas/Mix_Gammas/Code

# run the code `test.R`
Rscript simulation2.R
Rscript simulation3.R
Rscript simulation4.R


