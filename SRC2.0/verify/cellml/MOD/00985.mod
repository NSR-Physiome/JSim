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
  real z(time);
  real x(time);
  real y(time);

  // equations
  z = x(if (time<(time.min+1)) time.min else time-(1));
  when (time=time.min) x = 0;
  x:time = 1;
  y = x(if (time<(time.min+z)) time.min else time-(z));

  // variable properties
  z.sbmlRole="parameter";
  x.sbmlRole="parameter";
  y.sbmlRole="parameter";
}

