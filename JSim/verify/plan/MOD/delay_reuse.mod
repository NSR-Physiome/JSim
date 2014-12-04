// Delay is reused for ODE IC, s/b single time loop (2.02 fix)

math main {
  realDomain t;
  t.min=0; t.max=1; t.delta=1;
  real u(t) = t;
  real v(t) = if (t<1) 0 else u(t-1);
  real w(t);
  when (t=t.min) w=v;
  w:t = t;
}

