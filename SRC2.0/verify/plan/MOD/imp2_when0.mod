// 2 linear implicit eqns, 1 IC using when clause, 1 scalar

math main {
  realDomain t;
  t.min=0; t.max=4; t.delta=1;
  real a, c;
  realState b(t);
  a = 1;
  when (t=t.min) {
    b + c = a;
    b - c = 4;
  }
}
