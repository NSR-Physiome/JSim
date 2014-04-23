// PDE advection only, ambiguous x,t, no RHBC

math main {
  realDomain x,t;
  t.min=0; t.max=10; t.delta=1;
  x.min=0; x.max=1; x.delta=.1;

  real u(t,x);

  when (t=t.min) u=0;
  when (x=x.max) u=0;
  u:t = -u:x;
}

