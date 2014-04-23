// ODE with separately sequenced var for IC

math main {
  realDomain t;
  t.min=0; t.max=3; t.delta=.2;
  real v(t) = (t-1)^2;
  real u(t);
  when (t=t.min) u=v;
  u:t = -u;
}
