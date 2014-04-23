// vfc of algebraic var:  DELAY

import nsrunit;
unit conversion off;
math main {
  realDomain t min;
  t.min=0; t.max=10; t.delta=1;
  real d = 1 sec;
  real u(t) = t^2;
  real v(t) = if (t<d) u(t.min) else u(t-d);
}

