
all: wrap_actke.x 

clean:
	\rm -f *.xml *.o *.mod *.x *.optrpt


FC = pgf90 -acc -mp -byteswapio -Mlarge_arrays -fast -Minfo=accel,all,intensity,ccff -ta=tesla:managed -O3

wrap_actke.x: wrap_actke.o toto.o titi.o
	$(FC) -o wrap_actke.x wrap_actke.o toto.o titi.o

modi_titi.o: modi_titi.F90
	$(FC) -c modi_titi.F90

modi_toto.o: modi_toto.F90
	$(FC) -c modi_toto.F90

wrap_actke.o: $(MODULES) wrap_actke.F90 modi_toto.o
	$(FC) -c wrap_actke.F90

toto.o: $(MODULES) toto.F90 modi_titi.o
	$(FC) -c toto.F90

titi.o: $(MODULES) titi.F90 
	$(FC) -c titi.F90


