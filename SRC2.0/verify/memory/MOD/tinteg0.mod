math main {
  realDomain t;
  t.min=1; t.max=5; t.delta=1;
  real a(t) = t^2;
  real b = integral(a@t);
}
