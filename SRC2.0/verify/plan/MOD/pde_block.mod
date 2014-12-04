// PDE 1D advection-diffusion, mustepped vars

math main {
  realDomain t, x;
  t.min=0; t.max=3; t.delta=.1;
  x.min=0; x.max=1; x.delta=.05;
  real a(t) = t+1;
  real b(t) = t+2;
  real c(t) = t+3;
  real d(t) = t+4;
  real D(t)=1+a, B(t)=2+b, f1(t)=3+c, f2(t)=0, u0=0;
  real u(t,x);
  when (t=t.min) u=u0;
  when (x=x.min) f1*u=1;
  when (x=x.max) u:x=0;
  u:t = D*u:x:x - B*u:x + d;
}

