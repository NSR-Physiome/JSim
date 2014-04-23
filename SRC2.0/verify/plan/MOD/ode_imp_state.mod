// 2 ODEs with implicit state eqns

math main {
  realDomain t;
  t.min=0; t.max=5; t.delta=.2;
  real a, u(t), v(t);
  a = 1;
  when (t=t.min) {
    u = 1;
    v = 0;
  }
  u:t + v:t = u;
  u:t - v:t = v;
}
