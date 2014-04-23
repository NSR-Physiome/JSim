// ODE block mustep selection of outer loop var

math main {
  realDomain t, n;
  t.min=0; t.max=10; t.delta=2;
  n.min=1; n.max=3; n.delta=1;
  real a(t) = t^2;
  real b(n) = 5+n;
  real c(t,n) = a + b;
  real d(t,n) = t+n;
  real u(t,n);
  when (t=t.min) u=n;
  u:t = c + t;
}
