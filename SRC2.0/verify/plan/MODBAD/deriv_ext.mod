// symbolic deriv of extern

math main {
  realDomain t;
  t.min=0; t.max=1; t.delta=0.1;
  extern real u(t);
  real v(t) = u:t;
}

