// ODE block mustep selection of inner loop var  

math main {
  realDomain t, n;
  t.min=0; t.max=10; t.delta=2;
  n.min=1; n.max=3; n.delta=1;
  real a(t) = t^2;
  real b(t,n) = 5+n;
  real c(t) = a + b(t,n.max);
  real d(t,n) = t+n;
  real u(t);
  when (t=t.min) u=0;
  u:t = c + t;
}
