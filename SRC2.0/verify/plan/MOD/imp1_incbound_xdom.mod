// 1-eqn non-linear implicit (entire): bound not sequencable

math main {
  realDomain t;
  t.min=0; t.max=10; t.delta=1;
  real u;
  u^2 - 5*u + 6 = 0;
  u >= 0;
  u <= t+2.5;
}
