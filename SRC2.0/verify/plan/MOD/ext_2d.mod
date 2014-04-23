// 2D extern

math main {
  realDomain t, n;
  t.min=0; t.max=4; t.delta=1;
  n.min=1; n.max=3; n.delta=1;
  extern real u(t,n);
  real v(n,t) = u+1;
}
