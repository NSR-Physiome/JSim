math main {
  realDomain t;
  t.min=0; t.max=10; t.delta=.25;
  real u(t);
  when (t=t.min) u=1;
  u:t = -u;
}
