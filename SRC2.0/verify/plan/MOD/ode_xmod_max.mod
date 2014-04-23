// ODE with extra domain x, state eqn uses derived w(x.max)

math main {
  realDomain t, x;
  t.min=0; t.max=10; t.delta=1;
  x.min=0; x.max=1; x.ct=5;
  real u(t,x), v(t,x), w(t,x), wf(t);
  when (t=t.min) u = 1;
  u:t = -wf;
  v = t+x;
  w = u+v;
  when (x=x.max) wf = w;
}
