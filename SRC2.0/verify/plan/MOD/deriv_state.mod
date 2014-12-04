// symbolic deriv of state var

math main {
  realDomain t;
  t.min=0; t.max=5; t.delta=1;
  realState u(t);
  when (t=t.min) u=1;
  event (t>3) u = u+1;
  real v(t) = u:t;
}

