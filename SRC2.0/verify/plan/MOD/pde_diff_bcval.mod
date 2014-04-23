// PDE 1D diffusion, value BCs

math main {
  realDomain t, x;
  t.min=0; t.max=3; t.delta=.1;
  x.min=0; x.max=1; x.delta=.05;
  real u(t,x);
  when (t=t.min) u=0;
  when (x=x.min) u=t;
  when (x=x.max) u=0;
  u:t = u:x:x;
}

