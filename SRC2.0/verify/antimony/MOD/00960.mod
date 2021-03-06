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
  real P1;
  real P2;
  real P3;
  real P4;
  real P5;

  // equations
  P1 = 6.02214179E23;
  P2 = 6.02214179E23;
  P3 = 6.02214179E23;
  P4 = 6.02214179E23;
  P5 = 6.02214179E23-6.022099999999999E23;

  // variable properties
  P1.sbmlRole="parameter";
  P2.sbmlRole="parameter";
  P3.sbmlRole="parameter";
  P4.sbmlRole="parameter";
  P5.sbmlRole="parameter";
}

