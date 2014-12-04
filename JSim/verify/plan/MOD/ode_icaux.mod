// ODE with aux variable needed in both IC & loop

math main {
  realDomain t;
  t.min=0; t.max=5; t.delta=.2;
  real u(t), v(t), w(t);
  when (t=t.min) {
    u = 1;
    w = v/2;
  }
  u:t = -u;
  v = 2*u;
  w:t = -v;
}
