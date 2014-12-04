// ODE block mustep selection with extern

math main {
  realDomain t;
  t.min=0; t.max=10; t.delta=2;
  extern real a(t);
  real b = 5;
  real c(t) = a + b;
  real d(t) = t+1;
  real u(t);
  when (t=t.min) u=0;
  u:t = c + t;
}
