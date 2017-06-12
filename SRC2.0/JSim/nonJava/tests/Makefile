# Build/run recursion test mainlines for native solvers

include $(JSIMHOME)/lib/Makefile.global

TESTS=dopri5test radautest cvodetest simplxtest \
	dggopttest nl2soltest toms731test

#32BITFLAG= -m32 -shared
64BITFLAG= -m64 -shared
DOPRI5_ARGS=$(64BITFLAG) -o dopri5test dopri5test.c ../ode1/*o ../ode1/dopri5/dopri5.o -I ../ode1/dopri5 -lm
CVODE_ARGS=$(64BITFLAG) -o cvodetest cvodetest.c ../ode1/*o ../ode1/cvode/*.o -I ../ode1/cvode -lm
RADAU_ARGS=$(64BITFLAG) -o radautest  radautest.o ../ode1/radau/radau*.o 
SIMPLEX_ARGS=$(64BITFLAG) -o simplxtest  simplxtest.o ../opt/*.o ../opt/simplex/*.o
GGOPT_ARGS=$(64BITFLAG) -o dggopttest  dggopttest.o ../opt/*.o ../opt/ggopt/*.o 
NL2SOL_ARGS=$(64BITFLAG) -o nl2soltest  nl2soltest.o ../opt/*.o ../opt/nl2sol/*.o 
TOMS731_ARGS=$(64BITFLAG) -o toms731test  toms731test.o ../pde1/toms731.o ../pde1/ddassl.o

all: $(TESTS) 

# gfortran 
FLINK=$(CC)
FLDFLAGS= -lgfortran -lgfortranbegin 
#FLDFLAGS= -lgfortran 
FLDFLAGS=$(FLIB) $(FBEGLIB)
CLDFLAGS=

# g95 different
ifeq ($(JSIMOSARCH),linux_i386)
  FLINK=$(FORTRAN)
  FLDFLAGS=
  CLDFLAGS=$(LDFLAGS)
endif

cvodetest:
	$(CC) $(CVODE_ARGS) $(CFLAGS) 

dopri5test:
	$(CC) $(DOPRI5_ARGS) $(CFLAGS) 

radautest: radautest.o
	$(FLINK) $(RADAU_ARGS) $(FLDFLAGS)

simplxtest: simplxtest.o
	$(FLINK) $(SIMPLEX_ARGS) $(FLDFLAGS)

dggopttest: dggopttest.o
	$(FLINK) $(GGOPT_ARGS) $(FLDFLAGS)

nl2soltest: nl2soltest.o
	$(FLINK) $(NL2SOL_ARGS) $(FLDFLAGS)

toms731test: toms731test.o
	$(FLINK) $(TOMS731_ARGS) $(FLDFLAGS)

run:
	@for i in $(TESTS); do echo == running $$i; $$i | tail -1; done

clean:
	rm -f *.o $(TESTS)

