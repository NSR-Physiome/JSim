// D(x) => no LSFEA

math main {
  realDomain t, x;
  t.min=0; t.max=5; t.delta=1;
  x.min=0; x.max=1; x.delta=.1;
  
  real u(t,x);
  when (t=t.min) u=0;

  real B=1;
  real D(x)=x+1;
  real S=0;
  
  when (x=x.min) u=t;
  when (x=x.max) u:x=0;
  u:t = D*u:x:x - B*u:x + S;
}
