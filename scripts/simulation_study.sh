#!/bin/bash
#$ -V
#$ -q all.q
#$ -cwd
#$ -t 1-50
module load R/4.0.5
module load gnu-compilers/10.2.0
module unload gnu-compilers/4.9.4
Rscript simulation_study.R $SGE_TASK_ID 