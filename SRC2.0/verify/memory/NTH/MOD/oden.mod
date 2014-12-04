math main {
  realDomain t;
  t.min=0; t.max=10; t.delta=1;
  realDomain n;
  n.min=1; n.max=3; n.delta=3;
  
  real u(t,n);
  when (t=t.min) u=n;
  u:t = -u*n;
}
