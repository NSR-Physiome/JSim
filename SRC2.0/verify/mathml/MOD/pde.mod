math main {
  realDomain t, x;
  t.min=0; t.max=10; t.delta=.25;
  x.min=0; x.max=1;  x.delta=.1;
  real u(x,t);
  
  when (t=t.min) u=x;
  when (x=x.min) u=0;
  when (x=x.max) u:x=0;
  u:t = u:x:x;
}
