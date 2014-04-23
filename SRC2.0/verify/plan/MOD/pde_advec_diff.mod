// PDE 1D advection-diffusion, mixed BCs

math main {
  realDomain t, x;
  t.min=0; t.max=3; t.delta=.1;
  x.min=0; x.max=1; x.delta=.05;
  real D=1, B=1, f1=1, f2=1, u0=0;
  real u(t,x);
  when (t=t.min) u=u0;
  when (x=x.min) f1*u+f2*u:x=t;
  when (x=x.max) u:x=0;
  u:t = D*u:x:x - B*u:x;
}

