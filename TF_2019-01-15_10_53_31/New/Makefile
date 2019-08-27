TIMESTAMP=$(shell date +%F_%k_%M_%S)
FILE_TIMESTAMP=TF_$(shell echo ${TIMESTAMP} | sed 's/ //g')

.SUFFIXES: $(SUFFIXES) .f90 .F90 .c .o

PROG =	test-mf18

SRCS =	load_mod.F90 main_simple4_actke.F90 parkind1.F90 \
	run_simple4_actke.F90 simple4_acbl89.F90 simple4_acevolet.F90 \
	simple4_actke.F90 simple4_acturb.F90 simple4_fl2hl.F90 \
	simple4_hl2fl.F90

ARCH=GPU
ARCH=CPU

MODULES= parkind1.o load_mod.o

OBJS =	main_simple4_actke.o run_simple4_actke.o \
	simple4_acbl89.o simple4_acevolet.o simple4_actke.o simple4_acturb.o \
	simple4_fl2hl.o simple4_hl2fl.o

OBJS_LOAD =	main_simple4_actke.${ARCH}.o run_simple4_actke.${ARCH}.o \
	simple4_acbl89.${ARCH}.o simple4_acevolet.${ARCH}.o simple4_actke.${ARCH}.o simple4_acturb.${ARCH}.o \
	simple4_fl2hl.${ARCH}.o simple4_hl2fl.${ARCH}.o

LIBS = -L${CUDA_ROOT}/lib64 

CC = pgcc
CFLAGS = -fast
FC = pgfortran
FFLAGS = -fast
OMP=-acc -ta=multicore
OMP=
OMP=
OMP=-acc -ta=tesla:managed
OMP=-mp
OMP=-acc -ta=tesla -DD_LAST_LEVEL
OMP=-acc -ta=tesla:managed -DD_LAST_LEVEL
OMP=-acc -ta=tesla:managed -DNVTX -DD_LAST_LEVEL
OMP=-acc -ta=tesla:managed -DNVTX
#####
OMP=-acc -ta=multicore -DD_LAST_LEVEL -DNVTX
OMP=-acc -ta=multicore -DD_LAST_LEVEL
OMP=-mp
OMP=-acc -ta=multicore
OMP=-acc -ta=tesla:managed,cc70 -DD_LAST_LEVEL
OMP=-acc -ta=tesla:managed,cc70 -Mlarge_arrays

F90 = pgf90
#F90BASE = -fast -mcmodel=medium $(OMP) -byteswapio -Minfo=all -Minline=simple4_fl2hl -Minline=simple4_hl2fl,reshape
F90BASE = -fast -mcmodel=medium $(OMP) -byteswapio -Minfo=all -Minline,reshape -Mipa=inline
F90BASE = -fast $(OMP) -byteswapio -Minfo=all -Minline,reshape -Mipa=inline
PROFILE=-pg
PROFILE=
CUDA_OPTIONS=lineinfo
CUDA_COMPILE="-Mcuda=$(CUDA_OPTIONS)"
CUDA_LOAD=-Mcuda
CUDA_COMPILE=
CUDA_LOAD=
F90FLAGS = $(F90BASE) $(CUDA_COMPILE) -D${ARCH} $(PROFILE)
LDFLAGS = $(CUDA_LOAD) $(PROFILE) $(OMP)

all: $(MODULES) $(PROG)

$(PROG): $(OBJS) $(OBJS_LOAD)
	$(F90) $(LDFLAGS) -o $@ $(OBJS_LOAD) $(MODULES) $(LIBS)

clean:
	rm -f $(PROG) $(OBJS) $(OBJS_LOAD) $(MODULES) *.mod

nvtx.o:nvtx.f90
	$(F90) -Mpreprocess $(F90BASE) -c $<
parkind1.o:parkind1.F90
	$(F90) -Mpreprocess $(F90BASE) -c $<
load_mod.o:parkind1.o load_mod.F90
	$(F90) -Mpreprocess $(F90BASE) -c $*.F90
#simple4_hl2fl.o:simple4_hl2fl.F90
#	$(F90) -Mpreprocess $(F90BASE) $(CUDA_COMPILE) -DCPU $(PROFILE) -c -o $*.${ARCH}.o $<
#	cp $*.${ARCH}.o $*.o
	
main_simple4_actke.o:load_mod.o

.f90.o:
	$(F90) $(F90FLAGS) -c $<

.F90.o:
	$(F90) -Mpreprocess $(F90FLAGS) -c -o $*.${ARCH}.o $<
	cp $*.${ARCH}.o $*.o
#	$(F90) -Mpreprocess $(F90FLAGS) -c $<

tar:
#       @echo $(TIMESTAMP)
#       @echo $(FILE_TIMESTAMP)
	tar cvfz $(FILE_TIMESTAMP).tar.gz Makefile $(SRCS) nvtx.f90 *.h make makemake makemake.pl ../Orig/src.h.tgz  ../Orig/src.tar.gz ../run/test-mf18.sbatch
	chmod 400 $(FILE_TIMESTAMP).tar.gz
