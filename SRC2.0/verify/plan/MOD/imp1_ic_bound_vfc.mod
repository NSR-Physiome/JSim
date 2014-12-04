// 1-eqn non-linear implicit (entire) for IC, with vfc bounds

math main {
  realDomain t;
  t.min=0; t.max=3; t.delta=1;
  real u(t);
  when (t=t.min) 
    u^2 - 5*u + 6 = 0;
  u(t.min) >= 0;
  u(t.min) <= 10; 
  u:t = -u;
}
