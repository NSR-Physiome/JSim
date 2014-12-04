// 1st PDE example
math pde1 {
  realDomain t, x;
  t.min=0; t.max=3; t.delta=0.5;
  x.min=0; x.max=1; x.delta=0.1;
  real u(x,t);             // PDE state variable
  when (t=t.min) u = x^2;  // initial condition
  when (x=x.min) u = t^2;  // Left hand boundary conditon (LHBC)
  when (x=x.max) u:x = 0;  // Right hand boundary condition (RHBC)
  u:t = u:x:x - u;         // state equation
}
