// implcit eqn reused for t=t.min, jcode must reuse JImplicitWriter

math main {
  realDomain t;
  t.min=0; t.max=1; t.delta=1;
  real a(t);
  a^2 + a - 6 = t;
  real b(t);
  when (t=t.min) b = a;
  b:t = b;
}

  
