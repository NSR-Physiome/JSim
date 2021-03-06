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
  real comp1 = 10 L;
  real X1 = 22 mol;
  real X2 = 33 mol;
  real X3(time) mol;
  private real f1_call0(time);

  // equations
  X3 = f1_call0;
  f1_call0 = X1/X2;

  // Used function definitions
  // Function definition f1(A,B)=A/B;

  // variable properties
  comp1.sbmlRole="compartment";
  X1.sbmlRole="species";
  X1.sbmlCompartment="comp1";
  X2.sbmlRole="species";
  X2.sbmlCompartment="comp1";
  X3.sbmlRole="species";
  X3.sbmlCompartment="comp1";
}

