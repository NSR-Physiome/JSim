// 2 event vars with 2 time passes

math main {
  realDomain t;
  t.min=0; t.max=5; t.delta=.2;
  realState u(t), v(t);
  real uf;
  when (t=t.min) { u = 1; v = uf; }
  when (t=t.max) uf = u;
  event (t>1) u=u+1;
  event (t>2) v=v+1;
}
