
all: nproma.x 

clean:
	\rm -f *.xml *.o *.mod *.x *.optrpt


FC = pgf90 -DGPU  -mp -byteswapio -Mlarge_arrays -Mcuda -Mcuda=lineinfo  -O3

load_mod.o: parkind1.o load_mod.F90
	$(FC) -c load_mod.F90

parkind1.o: parkind1.F90
	$(FC) -c parkind1.F90

save_mod.o: parkind1.o save_mod.F90
	$(FC) -c save_mod.F90

nproma.x: nproma.F90 load_mod.o save_mod.o parkind1.o xrd_getoptions.o xrd_unix_env.o
	$(FC) -o nproma.x nproma.F90 load_mod.o save_mod.o parkind1.o xrd_getoptions.o xrd_unix_env.o

xrd_getoptions.o: xrd_getoptions.F90 parkind1.o xrd_unix_env.o
	$(FC) -c xrd_getoptions.F90

xrd_unix_env.o: xrd_unix_env.F90 parkind1.o
	$(FC) -c xrd_unix_env.F90
