// 3 ODEs with 3 time passes

math main {
  realDomain t;
  t.min=0; t.max=5; t.delta=.2;
  real u(t), v(t), w(t), uf, vf;
  real v1(t) = v+1;
  real v2(t) = v1+1;
  real w1(t) = w+1;
  real w2(t) = w1+1;
  when (t=t.min) { 
      u = 1; 
      v = uf; 
      w = vf;
  }
  when (t=t.max) {
      uf = u;
      vf = v2;
  }
  u:t = -u;
  v:t = -v;
  w:t = -w;
}
