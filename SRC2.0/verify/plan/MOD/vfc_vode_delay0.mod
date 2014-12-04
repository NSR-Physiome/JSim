// vfc delay used indirectly in ODE

math main {
  realDomain t;
  t.min=0; t.max=1; t.delta=.1;
  real u(t), v(t); 
  when (t=t.min) u=1;
  u:t = v; 
  v(t) = u(t-1);
}
