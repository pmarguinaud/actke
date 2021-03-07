
FC = gfortran -fconvert=big-endian -g -O2

nproma.x: nproma.o save_mod.o load_mod.o parkind1.o
	$(FC) -o nproma.x nproma.o save_mod.o load_mod.o

nproma.o: nproma.F90 save_mod.o load_mod.o
	$(FC) -c nproma.F90

save_mod.o: parkind1.o save_mod.F90
	$(FC) -c save_mod.F90

load_mod.o: parkind1.o load_mod.F90
	$(FC) -c load_mod.F90

parkind1.o: parkind1.F90
	$(FC) -c parkind1.F90
