// exponential decay via ODE
math main {
  realDomain t;
  t.min=0; t.max=5; t.delta=0.1;
  real u(t);
  when (t=t.min) u=1;
  u:t = -u;
}
