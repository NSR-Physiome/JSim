// 2 PDEs with exchange and Tom731 cross-spatial derivs

math main {
  realDomain t, x;
  t.min=0; t.max=3; t.delta=.1;
  x.min=0; x.max=1; x.delta=.05;
  real u(t,x), v(t,x);
  when (t=t.min) { u=0; v=0; }
  when (x=x.min) { u=t; v=t; }
  when (x=x.max) { u:x=0; v:x=0; }
  u:t = u:x:x + u - v:x;
  v:t = v:x:x + v - u:x;
}

