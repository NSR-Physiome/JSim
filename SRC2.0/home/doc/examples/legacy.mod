// use of legacy sum() and integral() operators
unit conversion on;
import nsrunit;
math legacy {
  realDomain x sec; x.min=0; x.max=1; x.delta=.1;
  real u(x) = x * (5 kg/sec); // u will have units kg
  real usum;
  usum = sum(u@x); // sum() has save units as u
  real uint;
  uint = integral(u@x); // integral() has product of u & x units
}
