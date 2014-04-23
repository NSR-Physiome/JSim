// ODE torder=2, only 2nd deriv present, no ICs

math main {
  realDomain t;
  t.min=0; t.max=2*PI; t.ct=13;
  real u(t);
  u:t:t + u = 0;
}
