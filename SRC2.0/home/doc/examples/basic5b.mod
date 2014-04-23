// 2 coupled ODEs
math main {
  realDomain t; 
  t.min=0; t.max=5; t.delta=0.1;
  real u(t), v(t);
  when (t=t.min) {
    u = 1;
    v = 2;
  }
  u:t = v-u;
  v:t = u-v;
}
