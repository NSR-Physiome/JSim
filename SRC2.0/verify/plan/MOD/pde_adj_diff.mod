// PDE diffusion: 2 adjacent tanks

math main {
  realDomain t, x;
  t.min=0; t.max=3; t.delta=.1;
  x.min=0; x.max=1; x.delta=.05;
  real u(t,x), v(t,x);
  when (t=t.min) { u=x; v=x.max-x; }
  when (x=x.min) u:x = 0;
  u(t, x.max) = v(t, x.min);
  when (x=x.max) v:x = 0;
  u:t = u:x:x;
  v:t = v:x:x;
}

