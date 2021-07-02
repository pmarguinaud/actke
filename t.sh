#!/bin/bash
#SBATCH --export=NONE
#SBATCH --nodes=1
#SBATCH --account=hun@gpu
#SBATCH --time 00:25:00
#SBATCH --exclusive
#SBATCH --gres=gpu:2


cd /gpfswork/rech/jau/ufh62jk/actke/openacc-kernels
module load nvidia-compilers/21.5

set -x
set -e

git=/gpfsstore/rech/jau/ufh62jk/install/git-2.32.0/bin/git

echo "============="

nsys profile -o openacc-kernels.qdrep ./compile.gpu/wrap_actke.x --case t1198 # --diff # --diff-block-list 57

exit

#if [ 0 -eq 1 ]
#then

./scripts/compile.pl --arch cpu --update  --compile --bin wrap_actke.x
./compile.cpu/wrap_actke.x --case t0031 # --diff # --diff-block-list 1 10 100

#else

./scripts/compile.pl --arch gpu --update  --compile --bin wrap_actke.x
./compile.gpu/wrap_actke.x --case t0031 # --diff # --diff-block-list 57

#fi
