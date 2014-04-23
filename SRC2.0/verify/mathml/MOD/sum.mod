math main {
  realDomain t;
  t.min=0; t.max=5; t.delta=1;
  real a(t) = t^2;

  real b(t) = sum(t=t.min to t.max, a);
}
