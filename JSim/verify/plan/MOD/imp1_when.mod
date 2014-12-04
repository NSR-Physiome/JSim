// 1-eqn non-linear implicit (when clause)

math main {
  realDomain t;
  t.min=0; t.max=1; t.delta=1;
  realState u(t);
  when (t=t.min)
     u^2 - 5*u + 6 = 0;
}
