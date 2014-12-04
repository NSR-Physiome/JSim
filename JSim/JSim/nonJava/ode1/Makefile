TARGET_LIB= $(LIBPFX)odesolver$(LIBSFX)
SRCS_F= odesubs.F
SRCS_c= jsodethread.c
SUBDIRS = radau dopri5 cvode
XOBJS = radau/*.o dopri5/*.o cvode/*.o 

lib: $(LOBJS) shlib

include ../Makefile.global.nml

