// imp eqns solving IC(u) and maintool(v)

math main {
  realDomain t;
  t.min=0; t.max=3; t.delta=.2;
  real u(t), v;
  when (t=t.min) {
     u(t.min) + v = 2;
     u(t.min) - v = 0;
  }
  u:t = v;
}
