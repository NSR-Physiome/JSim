// symbolic deriv: direct function of t, 2nd derivative

math main {
  realDomain t;
  t.min=0; t.max=1; t.delta=0.1;
  real u(t) = t^3;
  real v(t) = u:t:t;
}

