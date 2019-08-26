#!/bin/bash

set -x
set -e

module load pgi/18.10

\rm -f *.o *.x *.mod

ARCH=CPU
OPTS="-Mlarge_arrays"

pgf90 -mp -c -byteswapio -Mlarge_arrays parkind1.F90
pgf90 -mp -c -byteswapio -Mlarge_arrays load_mod.F90

for f in yomlog main_simple2_actke simple2_acbl89 simple2_acevolet simple2_actke simple2_acturb simple2_fl2hl simple2_hl2fl run_simple2_actke
do
  pgf90 $OPTS -D$ARCH -mp -c -byteswapio $f.F90 -o $f.$ARCH.o
done
pgf90 $OPTS -byteswapio -o main_simple2_actke.$ARCH.x *.$ARCH.o parkind1.o load_mod.o 

