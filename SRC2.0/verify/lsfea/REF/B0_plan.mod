// no LSFEA support, B=0 => f1,g1,f3,g3 = 0 (only f1 tested)

math main {
  realDomain t, x;
  t.min=0; t.max=5; t.delta=1;
  x.min=0; x.max=1; x.delta=.1;
  
  real u(t,x), v(t,x);
  when (t=t.min) { u=0; v=0; }

  real D=1;
  
  when (x=x.min) { u = t; v = t; }
  when (x=x.max) { u:x = 0; v:x = 0; } 
  u:t = D*u:x:x - 0*u:x + v-u;
  v:t = D*v:x:x - v:x + u-v;
}
