// 1-eqn non-linear implicit (entire) bounds with unit factor mult

import nsrunit;
unit conversion on;

math main {
  real b = -5 sec;
  real c = 6 sec^2;
  real umax = 1 min;
  real u sec;
  u^2 + b*u + c = 0;
  u >= 0;
  umax >= u;
}
