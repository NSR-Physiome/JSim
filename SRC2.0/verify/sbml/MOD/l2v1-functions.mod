// This model generated automatically from SBML

// unit definitions
import nsrunit;
unit conversion off;

// SBML property definitions
property sbmlRole=string;
property sbmlName=string;
property sbmlCompartment=string;

// SBML reactions
// reaction_1: S1 <=> S2 

math main {
  realDomain time second;
  time.min=0;
  extern time.max;
  extern time.delta;

  // variable definitions
  real compartmentOne = 1 L;
  real S1(time) M;
  real S2(time) M;
  real reaction_1(time) katal;
  private real f_call0(time);

  // equations
  when (time=time.min) S1 = 0;
  (S1*compartmentOne):time = -1*reaction_1;
  when (time=time.min) S2 = 0;
  (S2*compartmentOne):time = reaction_1;
  reaction_1 = f_call0;
  f_call0 = S1*2;

  // Used function definitions
  // Function definition f(x)=x*2;

  // variable properties
  compartmentOne.sbmlRole="compartment";
  S1.sbmlRole="species";
  S1.sbmlCompartment="compartmentOne";
  S2.sbmlRole="species";
  S2.sbmlCompartment="compartmentOne";
  reaction_1.sbmlRole="rate";
}

