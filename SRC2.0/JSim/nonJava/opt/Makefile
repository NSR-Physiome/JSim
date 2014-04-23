TARGET_LIB = $(LIBPFX)opt$(LIBSFX)
SRCS_f     = r1mach.f d1mach.f dcopy.f ddot.f dnrm2.f
SRCS_c     = jsoptthread.c
SUBDIRS    = simplex ggopt nl2sol
XOBJS = simplex/*.o ggopt/*.o nl2sol/*.o

lib: $(LOBJS) shlib

include ../Makefile.global.nml

