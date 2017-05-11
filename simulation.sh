#!/bin/bash

# The first line indicates the intepretor: bash/Rscript/sh/...



# MAX use of cpys and memory

#PBS -l nodes=1,ncpus=1,mem=4G,walltime=20:00:00:00

#PBS -q long

#PBS -M xch234@uky.edu

#PBS -m abe



# change dir to your working dir

cd /home/xch234/mix_gammas/Mix_Gammas/Code/2017_15_11/

# run the code `test.R`
Rscript Condition_1_100.R
Rscript Condition_1_250.R
Rscript Condition_1_500.R
Rscript Condition_10_100.R
Rscript Condition_10_250.R
Rscript Condition_10_500.R
Rscript Condition_11_100.R
Rscript Condition_11_250.R
Rscript Condition_11_500.R
Rscript Condition_12_100.R
Rscript Condition_12_250.R
Rscript Condition_12_500.R
Rscript Condition_13_100.R
Rscript Condition_13_250.R
Rscript Condition_13_500.R
Rscript Condition_14_100.R
Rscript Condition_14_250.R
Rscript Condition_14_500.R
Rscript Condition_15_100.R
Rscript Condition_15_250.R
Rscript Condition_15_500.R
Rscript Condition_2_100.R
Rscript Condition_2_250.R
Rscript Condition_2_500.R
Rscript Condition_3_100.R
Rscript Condition_3_250.R
Rscript Condition_3_500.R
Rscript Condition_4_100.R
Rscript Condition_4_250.R
Rscript Condition_4_500.R
Rscript Condition_5_100.R
Rscript Condition_5_250.R
Rscript Condition_5_500.R
Rscript Condition_6_100.R
Rscript Condition_6_250.R
Rscript Condition_6_500.R
Rscript Condition_7_100.R
Rscript Condition_7_250.R
Rscript Condition_7_500.R
Rscript Condition_8_100.R
Rscript Condition_8_250.R
Rscript Condition_8_500.R
Rscript Condition_9_100.R
Rscript Condition_9_250.R
Rscript Condition_9_500.R

