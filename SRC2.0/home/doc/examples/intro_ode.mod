// introductory ODE model
math main {
  realDomain t;
  t.min=0; t.max=4; t.delta=0.1;
  real rate = 1;
  real u(t);           // declaration of u
  when (t=t.min) u=1;  // initial condition for u
  u:t = -rate*u;       // ODE state equation for u
}
