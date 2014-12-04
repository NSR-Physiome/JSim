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
  real cell = 1 L;
  real tau = 1;
  real m = .5;
  real q = 1;
  real delta_t = 1;
  real P(time) M;

  // equations
  when (time=time.min) P = 0;
  P:time = (1/(1+m*P(if (time<(time.min+delta_t)) time.min else time-(delta_t))^q)-P)/tau;

  // variable properties
  cell.sbmlRole="compartment";
  tau.sbmlRole="parameter";
  m.sbmlRole="parameter";
  q.sbmlRole="parameter";
  delta_t.sbmlRole="parameter";
  P.sbmlRole="species";
  P.sbmlCompartment="cell";
}

