// PDE with implicit tool used on LHBC

math main {
  realDomain t, x;
  t.min=0; t.max=1; t.delta=1;
  x.min=0; x.max=1; x.delta=1;
  
  real a(t);
  a^2 - a + 6 = t;

  real u(x,t);
  when (t=t.min) u=a;
  when (x=x.min) u=a;
  when (x=x.max) u:x = 0;
  u:t = u:x:x;
}
