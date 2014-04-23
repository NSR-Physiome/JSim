// 1-eqn non-linear implicit (entire) bound unseq, retry without

math main {
  realDomain t;
  t.min=0; t.max=3; t.delta=1;
  real u(t);
  u^2 - 5*u + t = 0;
  real a;
  when (t=t.max) a = u;
  u >= 0;
  u <= a;
}
