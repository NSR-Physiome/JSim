// symbolic deriv if 1-tool implicit eqn

math main {
  realDomain t;
  t.min=0; t.max=1; t.delta=0.1;
  real u(t), v(t);
  u + sin(u) = t;
  v = u:t;
}

