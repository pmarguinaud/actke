#!/bin/bash

set -x
set -e

module load intel/17.0.4.196

\rm -f *.o *.mod

OPTS="-convert big_endian -assume byterecl -qopenmp -fp-model source -g -O2 -ip -xAVX -ftz  -fast-transcendentals"

ifort $OPTS -c parkind1.F90 
ifort $OPTS -c load_mod.F90
ifort $OPTS -c save_mod.F90

for f in nproma
do
  ifort $OPTS -qopenmp -c $f.F90 
done

ifort -Wl,-rpath,/softs/intel/compilers_and_libraries_2017.4.196/linux/mkl/lib/intel64 \
  -Wl,-rpath,/softs/intel/compilers_and_libraries_2017.4.196/linux/compiler/lib/intel64_lin \
  $OPTS -o nproma.x *.o 

