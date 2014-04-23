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
// reaction2: S2 <=> S1 

math main {
  realDomain time second;
  time.min=0;
  extern time.max;
  extern time.delta;

  // variable definitions
  real compartment = 1 L;
  real k1 = .5;
  real k2 = .25;
  real S1 M;
  real S2(time) M;
  real reaction1(time) katal;
  real reaction2(time) katal;

  // equations
  S1 = .015/compartment;
  when (time=time.min) S2 = .015/compartment;
  (S2*compartment):time = reaction1 + -1*reaction2;
  reaction1 = compartment*k1*S1;
  reaction2 = compartment*k2*S2;

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

