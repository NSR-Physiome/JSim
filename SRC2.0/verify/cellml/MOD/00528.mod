// This model generated automatically from SBML

// unit definitions
import nsrunit;
unit conversion off;

// SBML property definitions
property sbmlRole=string;
property sbmlName=string;
property sbmlCompartment=string;

// SBML reactions
// reaction1: S1 <=> S2 

math main {
  realDomain time second;
  time.min=0;
  extern time.max;
  extern time.delta;

  // variable definitions
  real C = 1 L;
  real k1;
  private real division_call0(time);
  real k2 = 50;
  real S1(time) M;
  real S2(time) M;
  real reaction1(time) katal;

  // equations
  when (time=time.min) k1 = division_call0;
  division_call0 = k2/100;
  when (time=time.min) S1 = 1/C;
  (S1*C):time = -1*reaction1;
  when (time=time.min) S2 = 1.5/C;
  (S2*C):time = reaction1;
  reaction1 = C*k1*S1;

  // Used function definitions
  // Function definition division(x,y)=x/y;

  // variable properties
  C.sbmlRole="compartment";
  k1.sbmlRole="parameter";
  k2.sbmlRole="parameter";
  S1.sbmlRole="species";
  S1.sbmlCompartment="C";
  S2.sbmlRole="species";
  S2.sbmlCompartment="C";
  reaction1.sbmlRole="rate";
}

