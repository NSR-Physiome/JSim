// This model generated automatically from SBML

// unit definitions
import nsrunit;
unit conversion off;

// SBML property definitions
property sbmlRole=string;
property sbmlName=string;
property sbmlCompartment=string;

// SBML reactions
// reaction1: S1 <=> 2S2 
// reaction2: 2S2 <=> S1 

math main {
  realDomain time second;
  time.min=0;
  extern time.max;
  extern time.delta;

  // variable definitions
  real compartment = .3 L;
  real k1 = .35;
  real k2 = 180;
  real S1(time) M;
  real S2(time) M;
  real reaction1(time) katal;
  real reaction2(time) katal;

  // equations
  when (time=time.min) S1 = 1.5E-4/compartment;
  (S1*compartment):time = -1*reaction1 + reaction2;
  when (time=time.min) S2 = 0;
  (S2*compartment):time = 2*reaction1 + -2*reaction2;
  reaction1 = compartment*k1*S1;
  reaction2 = compartment*k2*S2^2;

  // variable properties
  compartment.sbmlRole="compartment";
  k1.sbmlRole="parameter";
  k2.sbmlRole="parameter";
  S1.sbmlRole="species";
  S1.sbmlCompartment="compartment";
  S2.sbmlRole="species";
  S2.sbmlCompartment="compartment";
  reaction1.sbmlRole="rate";
  reaction2.sbmlRole="rate";
}

