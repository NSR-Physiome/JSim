// transcendental functions require dimensionless args
import nsrunit;
unit conversion on;
math sinewave {
  realDomain t sec;   // time in seconds
  t.min=0; t.max=2*PI; t.ct=33;
  real u(t) = sin(t/(1 sec));  // dimensionless u has period 2*PI seconds
}
