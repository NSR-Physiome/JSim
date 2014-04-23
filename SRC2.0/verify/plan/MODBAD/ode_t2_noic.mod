// ODE torder=2, no IC of u:t

math main {
  realDomain t;
  t.min=0; t.max=2*PI; t.ct=13;
  real u(t);
  when (t=t.min) u=1;
  u:t:t - u:t + u = 0;
}
