math main {
  realDomain t, x;
  x.min=0; x.max=1; x.delta=0.1;
  t.min=0; t.max=5; t.delta=0.2;
  
  real a = 1, b=1;
  real d(x) = a*(1-x^2) + b*x^2;
  
  real u(t,x);
  when (t=t.min) u=x+1;
  when (x=x.min) u:x = 0;
  when (x=x.max) u:x = 0;
  u:t = d*u:x:x;
}

