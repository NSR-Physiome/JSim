// 2 ODEs with 2 time passes circularly dependent

math main {
  realDomain t;
  t.min=0; t.max=5; t.delta=.2;
  real u(t), v(t), uf, vf;
  when (t=t.min) { u = vf; v = uf; }
  uf = u(t.max); 
  vf = v(t.max);
  u:t = -u;
  v:t = -v;
}
