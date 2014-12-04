// 2D ODEs with exchange, different time vars

math main {
  realDomain t, x;
  t.min=0; t.max=5; t.delta=.2;
  x.min=0; x.max=1; x.delta=.1;
  real u(t,x), v(t,x);
  when (t=t.min) u = 1;
  when (x=x.min) v = 0;
  u:t = v-u;
  v:x = u-v;
}
