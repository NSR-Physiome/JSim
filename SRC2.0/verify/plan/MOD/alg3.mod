math main {
  realDomain t;
  t.min=0; t.max=10; t.delta=1;
  real a(t) = 2+t;
  real b;
  when (t=t.max) a=b+1;
}
