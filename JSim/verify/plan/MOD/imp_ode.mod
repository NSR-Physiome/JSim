// ode requires previous implicit

math main {
  realDomain t;
  t.min=0; t.max=4; t.delta=1;
  real a, b(t), c(t);
  real u(t);
  a = 1;
  b + c = 10;
  b - c = 4;
  when (t=t.min) u=a;
  u:t = a + b;
}
