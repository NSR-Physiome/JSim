// This model generated automatically from SBML

// unit definitions
import nsrunit;
unit conversion off;

// SBML property definitions
property sbmlRole=string;
property sbmlName=string;
property sbmlCompartment=string;

// SBML reactions
// J0: => x 

math main {
  realDomain time second;
  time.min=0;
  extern time.max;
  extern time.delta;

  // variable definitions
  real default_compartment = 1 L;
  real y(time);
  real x(time) M;
  real J0(time) katal;

  // equations
  y = 2+x(if (time<(time.min+.2)) time.min else time-(.2));
  when (time=time.min) x = 3/default_compartment;
  (x*default_compartment):time = J0;
  J0 = 1;

  // variable properties
  default_compartment.sbmlRole="compartment";
  y.sbmlRole="parameter";
  x.sbmlRole="species";
  x.sbmlCompartment="default_compartment";
  J0.sbmlRole="rate";
}

