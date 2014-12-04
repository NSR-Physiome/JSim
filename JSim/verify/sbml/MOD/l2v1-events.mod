// This model generated automatically from SBML
// WARNING: SBML events are not currently supported: 

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
  real cell = 1 L;
  real k1 = 1;
  real k2 = 0;
  real tau = .25;
  real P1(time) M;
  real P2(time) M;

  // equations
  when (time=time.min) P1 = 0;
  P1:time = k1-P1;
  when (time=time.min) P2 = 0;
  P2:time = k2-P2;

  // variable properties
  cell.sbmlRole="compartment";
  k1.sbmlRole="parameter";
  k2.sbmlRole="parameter";
  tau.sbmlRole="parameter";
  P1.sbmlRole="species";
  P1.sbmlCompartment="cell";
  P2.sbmlRole="species";
  P2.sbmlCompartment="cell";
}

