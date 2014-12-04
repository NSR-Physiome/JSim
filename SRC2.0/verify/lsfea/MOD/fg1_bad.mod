// no LSFEA support, at least one of f1, g1 must be zero

math main {
  realDomain t, x;
  t.min=0; t.max=5; t.delta=1;
  x.min=0; x.max=1; x.delta=.1;
  
  real u(t,x);
  when (t=t.min) u=0;

  real K1 = 1 ;
  real D=1;
  real B=1;
  real S=0;
  
  when (x=x.min) K1*u + u:x =0;
  when (x=x.max) u=t;
  u:t = D*u:x:x - B*u:x + S;
}
