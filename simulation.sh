#!/bin/bash

# The first line indicates the intepretor: bash/Rscript/sh/...



# MAX use of cpys and memory

#PBS -l nodes=1,ncpus=30,mem=4G,walltime=48:00:00

#PBS -q default

#PBS -M xch234@uky.edu

#PBS -m abe



# change dir to your working dir

cd /home/xch234/mix_gammas/Mix_Gammas/Code

# run the code `test.R`
Rscript simulation4.R
