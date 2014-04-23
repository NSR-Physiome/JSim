// vfc MAX used directly in ODE - should fail

math main {
  realDomain t;
  t.min=0; t.max=1; t.delta=.1;
  real u(t); when (t=t.min) u=1;
  u:t = u(t.max);
}
