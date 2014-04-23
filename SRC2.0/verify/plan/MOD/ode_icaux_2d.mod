// 2D ODEs with aux variable needed in both IC & loop

math main {
  realDomain t, x;
  t.min=0; t.max=5; t.delta=.2;
  x.min=0; x.max=1; x.ct=6;
  real u(t), v(t,x), w1(t,x), w2(t,x);
  when (t=t.min) {
    u = 1;
    w1 = v/2;
  }
  when (x=x.min)
    w2 = v/2;
  u:t = -u;
  v = u + x;
  w1:t = -v;
  w2:x = -v;
}
