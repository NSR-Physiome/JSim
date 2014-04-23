// 3 ODEs with 3 time passes

math main {
  realDomain t;
  t.min=0; t.max=5; t.delta=.2;
  real u(t), v(t), w(t), uf, vf;
  when (t=t.min) { 
      u = 1; 
      v = uf; 
      w = vf;
  }
  when (t=t.max) {
      uf = u;
      vf = v;
  }
  u:t = -u;
  v:t = -v;
  w:t = -w;
}
