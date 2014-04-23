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

math main {
  realDomain time second;
  time.min=0;
  extern time.max;
  extern time.delta;

  // variable definitions
  real compartment = 1 L;
  real k1 = 1;
  real S1(time) M;
  real S2(time) M;
  real reaction1(time) katal;
  private real multiply_call0(time);

  // equations
  when (time=time.min) S1 = .0015/compartment;
  (S1*compartment):time = -1*reaction1;
  when (time=time.min) S2 = .0015/compartment;
  (S2*compartment):time = reaction1;
  reaction1 = compartment*multiply_call0;
  multiply_call0 = k1*S1;

  // Used function definitions
  // Function definition multiply(x,y)=x*y;

  // variable properties
  compartment.sbmlRole="compartment";
  k1.sbmlRole="parameter";
  S1.sbmlRole="species";
  S1.sbmlCompartment="compartment";
  S2.sbmlRole="species";
  S2.sbmlCompartment="compartment";
  reaction1.sbmlRole="rate";
}

