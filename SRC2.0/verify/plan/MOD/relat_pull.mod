// relation causes t-phase pull
math main {
  realDomain t;
  t.min=0; t.max=10; t.delta=1;
  real u(t) = t^2;
  u <= u(t.max);
}
