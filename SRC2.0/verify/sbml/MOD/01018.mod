// This model generated automatically from SBML

// unit definitions
import nsrunit;
unit conversion off;

// SBML property definitions
property sbmlRole=string;
property sbmlName=string;
property sbmlCompartment=string;

// SBML reactions
// reaction1: S2 => S1 

math main {
  realDomain time second;
  time.min=0;
  extern time.max;
  extern time.delta;

  // variable definitions
  real C = 1 L;
  real kf = .9;
  real kr = .075;
  real S1 M;
  real S2(time) M;
  real reaction1(time) katal;

  // equations
  S1 = 1/C;
  when (time=time.min) S2 = 0;
  (S2*C):time = -1*reaction1;
  reaction1 = (-1)*(C*(kf*S1+(-1)*kr*S2));

  // variable properties
  C.sbmlRole="compartment";
  kf.sbmlRole="parameter";
  kr.sbmlRole="parameter";
  S1.sbmlRole="species";
  S1.sbmlCompartment="C";
  S2.sbmlRole="species";
  S2.sbmlCompartment="C";
  reaction1.sbmlRole="rate";
}

