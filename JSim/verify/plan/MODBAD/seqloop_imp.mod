// seqloops error: implicit eqns
math main {
  realDomain t;
  t.min=0; t.max=4; t.delta=1;
  real a, b;
  real c(t) = t^2;
  a + b = c;
  a - b = 0;
}
