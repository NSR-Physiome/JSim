// symbolic deriv if 1-tool implicit eqn

math main {
  realDomain t;
  t.min=0; t.max=1; t.delta=0.1;
  real u(t), v(t), w(t);
  u + v = t^2;
  u - v = t;
  w = u:t;
}

