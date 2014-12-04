// 2 ODEs with recirculation

math main {
  realDomain t;
  t.min=0; t.max=5; t.delta=.2;
  real Cin(t) = sin(t);
  real u(t), v(t);
  when (t=t.min) {
    u = 1;
    v = 0;
  }
  u:t = Cin+v-u;
  v:t = u-v;
}
