#!/bin/bash
#SBATCH --export=NONE
#SBATCH --job-name=test-mf18
#SBATCH --mem=60000
#SBATCH --nodes=1
#SBATCH --ntasks=4
#SBATCH --ntasks-per-node=4
#SBATCH --threads-per-core=1
#SBATCH --partition=hsw_v100_32g
#SBATCH --time=00:30:00
#SBATCH --exclusive
#SBATCH --verbose
#SBATCH --no-requeue

set -x

export OMP_NUM_THREADS=1

nvprof --cpu-profiling on --cpu-profiling-mode flat --cpu-profiling-show-library on ./test-mf18
