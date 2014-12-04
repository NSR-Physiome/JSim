// 2 ODEs with 2 time passes

math main {
  realDomain t;
  t.min=0; t.max=5; t.delta=.2;
  real u(t), v(t), uf;
  when (t=t.min) { u = 1; v = uf; }
  when (t=t.max) uf = u;
  u:t = -u;
  v:t = -v;
}
