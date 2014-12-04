math main {
  realDomain t, x;
  t.min=0; t.max=10; t.delta=1;
  x.min=0; x.max=1; x.delta=0.25;
  
  real u(t,x);
  
  when (t=t.min) u = x;
  
  u:t = -u * (1 + x);
}

