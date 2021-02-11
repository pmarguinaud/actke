
all: wrap_actke.x 

clean:
	\rm -f *.xml *.o *.mod *.x *.optrpt


FRTFLAGS = -convert big_endian -assume byterecl -traceback -qopenmp -qopenmp-threadprivate compat -vec-report3 -fPIC
OPT_FRTFLAGS = -fp-model source -g -O2 -ip -xAVX
OPT_FRTFLAGS = -fp-model source -g -O2 -ip -check bounds -debug full
#FC = /home/gmap/mrpm/marguina/install/gmkpack_support/wrapper/I161150/ifort $(FRTFLAGS) $(OPT_FRTFLAGS) -DCPU


FC = pgf90 -mp -byteswapio -Mlarge_arrays -Minfo=mp -mp -O0 -g -DUSE_STACK
FC = pgf90 -mp -byteswapio -Mlarge_arrays -fast -Minfo=accel,all,intensity,ccff -ta=tesla:managed -O3 -DUSE_STACK

wrap_actke.x: wrap_actke.o run_simple4_actke.o simple4_acbl89.o  simple4_acevolet.o  simple4_actke.o  simple4_acturb.o  simple4_fl2hl.o  simple4_hl2fl.o parkind1.o load_mod.o xrd_unix_env.o xrd_getoptions.o
	$(FC) -o wrap_actke.x wrap_actke.o run_simple4_actke.o simple4_acbl89.o  simple4_acevolet.o  simple4_actke.o  simple4_acturb.o  simple4_fl2hl.o  simple4_hl2fl.o parkind1.o load_mod.o xrd_unix_env.o xrd_getoptions.o

MODULES = parkind1.o load_mod.o xrd_unix_env.o xrd_getoptions.o

xrd_unix_env.o: xrd_unix_env.F90
	$(FC) -c xrd_unix_env.F90

xrd_getoptions.o: xrd_getoptions.F90
	$(FC) -c xrd_getoptions.F90

parkind1.o: parkind1.F90
	$(FC) -c parkind1.F90

load_mod.o: load_mod.F90
	$(FC) -c load_mod.F90

wrap_actke.o: $(MODULES) wrap_actke.F90
	$(FC) -c wrap_actke.F90

run_simple4_actke.o: $(MODULES) run_simple4_actke.F90
	$(FC) -c run_simple4_actke.F90

simple4_acbl89.o: $(MODULES) simple4_acbl89.F90
	$(FC) -c simple4_acbl89.F90

simple4_acevolet.o: $(MODULES) simple4_acevolet.F90
	$(FC) -c simple4_acevolet.F90

simple4_actke.o: $(MODULES) simple4_actke.F90
	$(FC) -c simple4_actke.F90

simple4_acturb.o: $(MODULES) simple4_acturb.F90
	$(FC) -c simple4_acturb.F90

simple4_fl2hl.o: $(MODULES) simple4_fl2hl.F90
	$(FC) -c simple4_fl2hl.F90

simple4_hl2fl.o: $(MODULES) simple4_hl2fl.F90
	$(FC) -c simple4_hl2fl.F90

main_simple4_actke.o: $(MODULES) main_simple4_actke.F90
	$(FC) -c main_simple4_actke.F90

