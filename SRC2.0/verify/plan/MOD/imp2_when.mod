// 2 linear implicit IC eqns using when clause

math main {
  realDomain t;
  t.min=0; t.max=4; t.delta=1;
  real a;
  realState b(t), c(t);
  a = 1;
  when (t=t.min) {
    b + c = a;
    b - c = 4;
  }
}
