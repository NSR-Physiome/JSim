// This model generated automatically from SBML

// unit definitions
import nsrunit;
unit conversion off;

// SBML property definitions
property sbmlRole=string;
property sbmlName=string;
property sbmlCompartment=string;

// SBML reactions
// J0: => S1 
// J1: 2S1 => 3S2 
// J2: S2 

math main {
  realDomain time second;
  time.min=0;
  extern time.max;
  extern time.delta;

  // variable definitions
  real default_compartment = 1 L;
  real k1 = 1;
  real k2 = 3;
  real k3 = 1.4;
  real S1conv = 3;
  real modelconv = 4;
  real S1(time) M;
  real S2(time) M;
  real J0(time) katal;
  real J1(time) katal;
  real J2(time) katal;

  // equations
  when (time=time.min) S1 = 0;
  (S1*default_compartment):time = modelconv*(J0 + -2*J1);
  when (time=time.min) S2 = .001/default_compartment;
  (S2*default_compartment):time = modelconv*(3*J1 + -1*J2);
  J0 = k1;
  J1 = k2*S1/S2;
  J2 = k3*S2;

  // variable properties
  default_compartment.sbmlRole="compartment";
  k1.sbmlRole="parameter";
  k2.sbmlRole="parameter";
  k3.sbmlRole="parameter";
  S1conv.sbmlRole="parameter";
  modelconv.sbmlRole="parameter";
  S1.sbmlRole="species";
  S1.sbmlCompartment="default_compartment";
  S2.sbmlRole="species";
  S2.sbmlCompartment="default_compartment";
  J0.sbmlRole="rate";
  J1.sbmlRole="rate";
  J2.sbmlRole="rate";
}

