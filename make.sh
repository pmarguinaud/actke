#!/bin/bash

set -x
set -e

if [ 1 -eq 1 ]
then

module load pgi/17.7

\rm -f *.o *.x *.mod

ARCH=GPU
#OPTS="-Mcuda=maxregcount:60 -Mcuda=lineinfo"
OPTS="-Mcuda -Mcuda=lineinfo"

pgf90 -mp -c -byteswapio parkind1.F90
pgf90 -mp -c -byteswapio load_mod.F90

for f in yomlog main_simple4_actke simple4_acbl89 simple4_acevolet simple4_actke simple4_acturb simple4_fl2hl simple4_hl2fl run_simple4_actke
do
  pgf90 $OPTS -D$ARCH -mp -c -byteswapio $f.F90 -o $f.$ARCH.o
done
pgf90 $OPTS -byteswapio -o main_simple4_actke.$ARCH.x *.$ARCH.o parkind1.o load_mod.o 

fi

module load intel/17.0.4.196

\rm -f *.o *.mod

ARCH=CPU
OPTS="-convert big_endian -assume byterecl -qopenmp -fp-model source -g -O2 -ip -xAVX -ftz  -fast-transcendentals"

ifort $OPTS -c parkind1.F90 
ifort $OPTS -c load_mod.F90

for f in yomlog main_simple4_actke simple4_acbl89 simple4_acevolet simple4_actke simple4_acturb simple4_fl2hl simple4_hl2fl run_simple4_actke
do
  ifort $OPTS -D$ARCH -qopenmp -c $f.F90 -o $f.$ARCH.o
done
ifort -Wl,-rpath,/softs/intel/compilers_and_libraries_2017.4.196/linux/mkl/lib/intel64 \
  -Wl,-rpath,/softs/intel/compilers_and_libraries_2017.4.196/linux/compiler/lib/intel64_lin \
  $OPTS -o main_simple4_actke.$ARCH.x *.$ARCH.o parkind1.o load_mod.o

