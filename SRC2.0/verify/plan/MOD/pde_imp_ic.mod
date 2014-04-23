// 2 PDEs with implicit ICs

math main {
  realDomain t, x;
  t.min=0; t.max=5; t.delta=.2;
  x.min=0; x.max=1; x.delta=.05;
  real a =1;
  real u(t,x), v(t,x);
  when (t=t.min) {
    u + v = a^2;
    u - v = a;
  }
  when (x=x.min) { u:x=0; v:x=0; }
  when (x=x.max) { u:x=0; v:x=0; }
  u:t = u:x:x + v - u;
  v:t = v:x:x + u - v;
}
