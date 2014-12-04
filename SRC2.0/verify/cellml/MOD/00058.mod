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
// reaction2: S2 <=> S3 

math main {
  realDomain time second;
  time.min=0;
  extern time.max;
  extern time.delta;

  // variable definitions
  real compartment = 1 L;
  real k = 1;
  real S1(time) M;
  real S2(time) M;
  real S3(time) M;
  real reaction1(time) katal;
  real reaction2(time) katal;
  real reaction2_k = 2;

  // equations
  when (time=time.min) S1 = 3E-4/compartment;
  (S1*compartment):time = -1*reaction1;
  when (time=time.min) S2 = 0;
  (S2*compartment):time = reaction1 + -1*reaction2;
  when (time=time.min) S3 = 0;
  (S3*compartment):time = reaction2;
  reaction1 = compartment*k*S1;
  reaction2 = compartment*reaction2_k*S2;

  // variable properties
  compartment.sbmlRole="compartment";
  k.sbmlRole="parameter";
  S1.sbmlRole="species";
  S1.sbmlCompartment="compartment";
  S2.sbmlRole="species";
  S2.sbmlCompartment="compartment";
  S3.sbmlRole="species";
  S3.sbmlCompartment="compartment";
  reaction1.sbmlRole="rate";
  reaction2.sbmlRole="rate";
  reaction2_k.sbmlRole="parameter";
}

