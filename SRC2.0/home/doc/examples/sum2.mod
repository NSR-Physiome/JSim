// "summing out" one variable domain
unit conversion on;
import nsrunit;
math sum2 {
  realDomain t sec; t.min=0; t.max=6; t.delta=1;
  realDomain x m; x.min=0; x.max=1; x.delta=0.1;
  real u(t,x) = t^2*(1 mole/sec^2) + x^2*(1 mole/m^2); // u in mole
  real v(t) = sum(x=x.min to x.max, u); // v in mole
}
