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
  real k1 = 1;
  real S1(time) M;
  real S2(time) M;
  real reaction1(time) katal;

  // equations
  when (time=time.min) S1 = 1.5E-4;
  (S1*C):time = -1*reaction1;
  when (time=time.min) S2 = 0;
  (S2*C):time = reaction1;
  reaction1 = C*k1*S1;

  // variable properties
  C.sbmlRole="compartment";
  k1.sbmlRole="parameter";
  S1.sbmlRole="species";
  S1.sbmlCompartment="C";
  S2.sbmlRole="species";
  S2.sbmlCompartment="C";
  reaction1.sbmlRole="rate";
}

