// PDE 1D diffusion, deriv BCs

math main {
  realDomain t, x, y;
  t.min=0; t.max=3; t.delta=.1;
  x.min=0; x.max=1; x.delta=.05;
  y.min=0; y.max=5; y.delta=1;
  real u(t,x,y);
  when (t=t.min) u=0;
  when (x=x.min) u:x=0;
  when (x=x.max) u:x=0;
  u:t = u:x:x;
}

