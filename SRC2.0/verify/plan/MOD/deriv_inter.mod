// symbolic deriv requiring intermediary variable

math main {
  realDomain t;
  t.min=0; t.max=1; t.delta=0.1;
  real u(t) = t^2;
  real v(t) = u + u^2;
  real w(t) = v:t;
}

