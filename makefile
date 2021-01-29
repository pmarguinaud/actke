
all: wrap_actke.x 

clean:
	\rm -f *.xml *.o *.mod *.x *.optrpt


FRTFLAGS = -convert big_endian -assume byterecl -traceback -qopenmp -qopenmp-threadprivate compat -fPIC
OPT_FRTFLAGS = -fp-model source -g -O2 -ip -xAVX
OPT_FRTFLAGS = -fp-model source -g -O2 -ip -check bounds -debug full
FC = /home/gmap/mrpm/marguina/install/gmkpack_support/wrapper/I185274/ifort $(FRTFLAGS) $(OPT_FRTFLAGS) -DCPU


#FC = pgf90 -DCPU  -mp -byteswapio -Mlarge_arrays -fast -O3 -g
#FC = pgf90 -DCPU  -mp -byteswapio -Mlarge_arrays -fast -Minfo=accel,all,intensity,ccff -ta=tesla:managed -O3

wrap_actke.x: wrap_actke.o run_simple2_actke.o simple2_acbl89.o  simple2_acevolet.o  simple2_actke.o  simple2_acturb.o  simple2_fl2hl.o  simple2_hl2fl.o parkind1.o load_mod.o yomlog.o xrd_unix_env.o xrd_getoptions.o
	$(FC) -o wrap_actke.x wrap_actke.o run_simple2_actke.o simple2_acbl89.o  simple2_acevolet.o  simple2_actke.o  simple2_acturb.o  simple2_fl2hl.o  simple2_hl2fl.o parkind1.o load_mod.o yomlog.o xrd_unix_env.o xrd_getoptions.o

MODULES = parkind1.o yomlog.o load_mod.o xrd_unix_env.o xrd_getoptions.o

xrd_unix_env.o: xrd_unix_env.F90 parkind1.o
	$(FC) -c xrd_unix_env.F90

xrd_getoptions.o: xrd_getoptions.F90 parkind1.o xrd_unix_env.o
	$(FC) -c xrd_getoptions.F90

parkind1.o: parkind1.F90
	$(FC) -c parkind1.F90

yomlog.o: yomlog.F90
	$(FC) -c yomlog.F90

load_mod.o: load_mod.F90 parkind1.o
	$(FC) -c load_mod.F90


wrap_actke.o: $(MODULES) wrap_actke.F90
	$(FC) -c wrap_actke.F90

run_simple2_actke.o: $(MODULES) run_simple2_actke.F90
	$(FC) -c run_simple2_actke.F90

simple2_acbl89.o: $(MODULES) simple2_acbl89.F90
	$(FC) -c simple2_acbl89.F90

simple2_acevolet.o: $(MODULES) simple2_acevolet.F90
	$(FC) -c simple2_acevolet.F90

simple2_actke.o: $(MODULES) simple2_actke.F90
	$(FC) -c simple2_actke.F90

simple2_acturb.o: $(MODULES) simple2_acturb.F90
	$(FC) -c simple2_acturb.F90

simple2_fl2hl.o: $(MODULES) simple2_fl2hl.F90
	$(FC) -c simple2_fl2hl.F90

simple2_hl2fl.o: $(MODULES) simple2_hl2fl.F90
	$(FC) -c simple2_hl2fl.F90

