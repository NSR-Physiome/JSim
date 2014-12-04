// disjoint pull within t loop

math main {
  realDomain t, n, m;
  t.min=0; t.max=1; t.delta=1;
  n.min=0; n.max=1; n.delta=1;
  m.min=0; m.max=1; m.delta=1;
  real a(t,n) = t + n;
  real b1(t,m) = a(t, n.max) + m;
  real b2(t,m) = t + m;
  real c(t,n) = b2(t, m.max) + n;
}
