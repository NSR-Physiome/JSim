// linear implicit ODE state eqns
math implicit4 {
  realDomain t;
  t.min=0; t.max=10; t.delta=1;
  real u(t), v(t);
  when (t=t.min) {
    u=2;
    v=1;
  }
  u:t + v:t = u-v^2;
  u:t - v:t = u+v^2; 
}
