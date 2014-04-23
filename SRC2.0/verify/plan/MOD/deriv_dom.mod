// symbolic deriv of domain

math main {
  realDomain t, x;
  t.min=0; t.max=10; t.delta=1;
  x.min=0; x.max=10; x.delta=1;
  real v(t) = t:x;
}
