// relation for all time
math main {
  realDomain t;
  t.min=0; t.max=10; t.delta=1;
  real u(t) = t^2;
  u < 10;
}
