// unsupported when clause

math main {
  realDomain t;
  t.min=0; t.max=10; t.delta=1;
  real u(t);
  when (u<3) { u = 1; }
  u:t = -u;
}
