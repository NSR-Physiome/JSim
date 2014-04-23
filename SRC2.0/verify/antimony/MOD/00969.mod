// This model generated automatically from SBML

// unit definitions
import nsrunit;
unit conversion off;

// SBML property definitions
property sbmlRole=string;
property sbmlName=string;
property sbmlCompartment=string;

// SBML reactions
// J0: => XrefX 

math main {
  realDomain time second;
  time.min=0;
  extern time.max;
  extern time.delta;

  // variable definitions
  real default_compartment = 1 L;
  real k1 = 1;
  real X(time) M;
  real Xref;
  real J0(time) katal;

  // equations
  when (time=time.min) X = 0;
  (X*default_compartment):time = Xref*J0;
  Xref = 3;
  J0 = k1;

  // variable properties
  default_compartment.sbmlRole="compartment";
  k1.sbmlRole="parameter";
  X.sbmlRole="species";
  X.sbmlCompartment="default_compartment";
  Xref.sbmlRole="species reference";
  J0.sbmlRole="rate";
}

