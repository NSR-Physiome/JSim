math main {
  realDomain t;
  t.min=0; t.max=10; t.delta=1;
  real a(t) = t^2;
  real b(t) = a+1;
  real u(t);
  when (t=t.min) u=b;
  u:t=-u;
}
