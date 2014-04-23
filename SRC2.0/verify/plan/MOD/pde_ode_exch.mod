// ODE/PDE tanks with exchange

math main {
  realDomain t, x;
  t.min=0; t.max=3; t.delta=.1;
  x.min=0; x.max=1; x.delta=.05;
  real u(t,x), v(t);
  when (t=t.min) { u=x; v=0; }
  when (x=x.min) u:x=0;
  when (x=x.max) u:x=0;
  u:t = u:x:x + v - u;
  v:t = integral(u@x) - v;
}

