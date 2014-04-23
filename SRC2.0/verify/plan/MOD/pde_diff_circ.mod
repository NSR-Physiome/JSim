// PDE 1D circular diffusion

math main {
  realDomain t, x;
  t.min=0; t.max=3; t.delta=.1;
  x.min=0; x.max=1; x.delta=.05;
  real u(t,x);
  when (t=t.min) u=abs(x-0.5);
  u(t, x.min) = u(t, x.max);
  u:t = u:x:x;
}

