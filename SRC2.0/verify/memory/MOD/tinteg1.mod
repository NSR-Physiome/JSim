math main {
  realDomain t;
  t.min=1; t.max=3; t.delta=1;
  real a(t) = t^2;
  real b(t) = integral(t=t.min to t, a);
}
