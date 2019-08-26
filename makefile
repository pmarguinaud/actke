
all: main_simple4_actke.x 
#all: main_simple4_actke.x main_simple2_actke.x

clean:
	\rm -f *.xml *.o *.mod *.x *.optrpt


#FRTFLAGS = -convert big_endian -assume byterecl -traceback -qopenmp -qopenmp-threadprivate compat -vec-report3 -fPIC
#OPT_FRTFLAGS = -fp-model source -g -O2 -ip -xAVX
#OPT_FRTFLAGS = -fp-model source -g -O2 -ip -check bounds -debug full
#FC = /home/gmap/mrpm/marguina/install/gmkpack_support/wrapper/I161150/ifort $(FRTFLAGS) $(OPT_FRTFLAGS) -DCPU


FC = pgf90 -DGPU  -mp -byteswapio -Mlarge_arrays -Mcuda -Mcuda=lineinfo 

main_simple2_actke.x: main_simple2_actke.o run_simple2_actke.o simple2_acbl89.o  simple2_acevolet.o  simple2_actke.o  simple2_acturb.o  simple2_fl2hl.o  simple2_hl2fl.o parkind1.o load_mod.o yomlog.o
	$(FC) -o main_simple2_actke.x main_simple2_actke.o run_simple2_actke.o simple2_acbl89.o  simple2_acevolet.o  simple2_actke.o  simple2_acturb.o  simple2_fl2hl.o  simple2_hl2fl.o parkind1.o load_mod.o yomlog.o

main_simple4_actke.x: main_simple4_actke.o run_simple4_actke.o simple4_acbl89.o  simple4_acevolet.o  simple4_actke.o  simple4_acturb.o  simple4_fl2hl.o  simple4_hl2fl.o parkind1.o load_mod.o yomlog.o
	$(FC) -o main_simple4_actke.x main_simple4_actke.o run_simple4_actke.o simple4_acbl89.o  simple4_acevolet.o  simple4_actke.o  simple4_acturb.o  simple4_fl2hl.o  simple4_hl2fl.o parkind1.o load_mod.o yomlog.o

MODULES = parkind1.o yomlog.o load_mod.o

parkind1.o: parkind1.F90
	$(FC) -c parkind1.F90

yomlog.o: yomlog.F90
	$(FC) -c yomlog.F90

load_mod.o: load_mod.F90
	$(FC) -c load_mod.F90


main_simple4_actke.o: $(MODULES) main_simple4_actke.F90
	$(FC) -c main_simple4_actke.F90

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

main_simple2_actke.o: $(MODULES) main_simple2_actke.F90
	$(FC) -c main_simple2_actke.F90

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

