// ODE torder=3, only 3nd deriv present

math main {
  realDomain t;
  t.min=0; t.max=2*PI; t.ct=13;
  real u(t);
  when (t=t.min) { u=1; u:t=0; u:t:t=-1;}
  u:t:t:t = -u;
}
