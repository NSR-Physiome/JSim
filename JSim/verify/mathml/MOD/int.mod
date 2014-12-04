math main {
  realDomain t;
  t.min=0; t.max=10; t.delta=.25;
  real a(t) = t^2;

  real b(t) = integral(t=t.min to t, a);

// integral(a@t) not ready yet
}
