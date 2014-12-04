// ODE torder=2, no ICs

math main {
  realDomain t;
  t.min=0; t.max=2*PI; t.ct=13;
  real u(t);
  u:t:t - u:t + u = 0;
}
