// This model generated automatically from SBML

// unit definitions
import nsrunit;
unit conversion off;

// SBML property definitions
property sbmlRole=string;
property sbmlName=string;
property sbmlCompartment=string;

// SBML reactions
// reaction_1: S1 S2 <=> S4 

math main {
  realDomain time second;
  time.min=0;
  extern time.max;
  extern time.delta;

  // variable definitions
  real compartmentOne = 1 L;
  real k1 = .5;
  real k2 = .1;
  real S1(time) M;
  real S2 = 1 M;
  real S3 = 3 M;
  real S4(time) M;
  real reaction_1(time) katal;

  // equations
  when (time=time.min) S1 = 0;
  S1:time = k2;
  when (time=time.min) S4 = 0;
  (S4*compartmentOne):time = reaction_1;
  reaction_1 = k1*S1*S2*S3;

  // variable properties
  compartmentOne.sbmlRole="compartment";
  k1.sbmlRole="parameter";
  k2.sbmlRole="parameter";
  S1.sbmlRole="species";
  S1.sbmlCompartment="compartmentOne";
  S2.sbmlRole="species";
  S2.sbmlCompartment="compartmentOne";
  S3.sbmlRole="species";
  S3.sbmlCompartment="compartmentOne";
  S4.sbmlRole="species";
  S4.sbmlCompartment="compartmentOne";
  reaction_1.sbmlRole="rate";
}

