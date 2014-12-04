// seqloops error: single expr
math main {
  realDomain t;
  t.min=0; t.max=4; t.delta=1;
  real a(t) = t^2;
  real b = a;
}
