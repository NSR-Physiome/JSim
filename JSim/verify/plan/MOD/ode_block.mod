// ODE block mustep selection

math main {
  realDomain t;
  t.min=0; t.max=10; t.delta=2;
  real a(t) = t^2;
  real b(t) = 5;
  real c(t) = a + b;
  real d(t) = t+1;
  real u(t);
  when (t=t.min) u=0;
  u:t = c + t;
}
