// 2 linear implicit IC eqns using VFC

math main {
  realDomain t;
  t.min=0; t.max=4; t.delta=1;
  real a;
  realState b(t), c(t);
  a = 1;
  b(t.min) + c(t.min) = a;
  b(t.min) - c(t.min) = 4;
}
