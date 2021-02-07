
all: wrap_actke.x 

clean:
	\rm -f *.xml *.o *.mod *.x *.optrpt


FRTFLAGS = -convert big_endian -assume byterecl -traceback -qopenmp -qopenmp-threadprivate compat -vec-report3 -fPIC
OPT_FRTFLAGS = -fp-model source -g -O2 -ip -xAVX
OPT_FRTFLAGS = -fp-model source -g -O2 -ip -check bounds -debug full
#FC = /home/gmap/mrpm/marguina/install/gmkpack_support/wrapper/I161150/ifort $(FRTFLAGS) $(OPT_FRTFLAGS) -DCPU


FC = pgf90 -DCPU  -mp -byteswapio -Mlarge_arrays -Minfo=mp -mp -O3 -g
FC = pgf90 -DCPU  -mp -byteswapio -Mlarge_arrays -Minfo=mp -mp -O0 -g
FC = pgf90 -DGPU  -mp -byteswapio -Mlarge_arrays -Mcuda -Mcuda=lineinfo  -O3

wrap_actke.x: wrap_actke.o run_actke.o actke.o  
	$(FC) -o wrap_actke.x wrap_actke.o run_actke.o actke.o  

wrap_actke.o: $(MODULES) wrap_actke.F90
	$(FC) -c wrap_actke.F90

run_actke.o: $(MODULES) run_actke.F90
	$(FC) -c run_actke.F90

actke.o: $(MODULES) actke.F90
	$(FC) -c actke.F90

