// linear implicit eqns in ODE ICs
math implicit3 {
  realDomain t; 
  t.min=0; t.max=10; t.delta=1;
  real u(t), v(t);
  when (t=t.min) {
    u + 2*v = 4;
    u - v = 1;
  }
  u:t = v-u;
  v:t = u-v;
}
