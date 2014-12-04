// 2-dimensional ODE
math main {
  realDomain t, x;
  t.min=0; t.max=5; t.delta=0.1;
  x.min=0; x.max=1; x.delta=0.1;
  real u(t, x);
  when (t=t.min) u=x; 
  u:t = -(u*x);   
}


