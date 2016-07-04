#!/bin/bash

# The first line indicates the intepretor: bash/Rscript/sh/...



# MAX use of cpys and memory

#PBS -l ncpus=30,mem=8G,walltime=24:00:00

#PBS -q short

#PBS -M jiaying.weng@uky.edu

#PBS -m abe



# change dir to your working dir

cd /home/xch234/mix_gammas/Mix_Gammas/Code

# run the code `test.R`
Rscript simulation.R

