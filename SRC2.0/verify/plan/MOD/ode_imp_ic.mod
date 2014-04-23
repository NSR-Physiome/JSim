// 2 ODEs with implicit ICs

math main {
  realDomain t;
  t.min=0; t.max=5; t.delta=.2;
  real a =1;
  real u(t), v(t);
  when (t=t.min) {
    u + v = a^2;
    u - v = a;
  }
  u:t = v-u;
  v:t = u-v;
}
