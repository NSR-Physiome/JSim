// symbolic deriv of PDE

math main {
  realDomain t, x;
  t.min=0; t.max=5; t.delta=0.5;
  x.min=0; x.max=1; x.delta=0.1;
  real u(t,x), v(t,x);
  when (t=t.min) u=x;
  when (x=x.min) u:x = 0;
  when (x=x.max) u:x = 0;
  u:t = u:x:x;
  v = u:t:t;
}

