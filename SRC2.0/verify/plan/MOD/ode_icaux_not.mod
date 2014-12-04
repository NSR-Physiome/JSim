// ODE with IC aux tool created, but unsequenced

math main {
  realDomain t;
  t.min=0; t.max=5; t.delta=.2;
  real u(t), v(t), w(t), uf;
  when (t=t.min) {
    u = 1;
    w = v + uf;
  }
  when (t=t.max)
    uf = u;
  u:t = -u;
  v = 2*u;
  w:t = -v;
}
