// simple use of sum() operator
unit conversion on;
import nsrunit;
math sum1 {
  realDomain t sec; t.min=0; t.max=5; t.delta=1; // time in seconds
  real u(t) = t * (3 m/sec); // u in meters
  real w = sum(t=t.min to t.max, u); // w in meters
}
