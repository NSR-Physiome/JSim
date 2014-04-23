// This model generated automatically from SBML

// unit definitions
import nsrunit;
unit conversion off;

// SBML property definitions
property sbmlRole=string;
property sbmlName=string;
property sbmlCompartment=string;

// SBML reactions

math main {
  realDomain time second;
  time.min=0;
  extern time.max;
  extern time.delta;

  // variable definitions
  real compartment = 1 L;
  real S1(time) M;

  // equations
  S1 = 7;

  // variable properties
  compartment.sbmlRole="compartment";
  S1.sbmlRole="species";
  S1.sbmlCompartment="compartment";
}

