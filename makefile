
all: wrap_actke.x 

clean:
	\rm -f *.xml *.o *.mod *.x *.optrpt


FC = pgf90 -acc -mp -byteswapio -Mlarge_arrays -fast -Minfo=accel,all,intensity,ccff -ta=tesla:managed -O3

wrap_actke.x: wrap_actke.o yomdata.o toto.o 
	$(FC) -o wrap_actke.x wrap_actke.o yomdata.o toto.o

modi_toto.o: modi_toto.F90
	$(FC) -c modi_toto.F90

wrap_actke.o: $(MODULES) wrap_actke.F90 yomdata.o modi_toto.o
	$(FC) -c wrap_actke.F90

yomdata.o: $(MODULES) yomdata.F90
	$(FC) -c yomdata.F90

toto.o: $(MODULES) toto.F90 yomdata.o
	$(FC) -c toto.F90


