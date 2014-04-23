math main {
  realDomain t, x;
  t.min=0; t.max=10; t.delta=.25;
  x.min=0; x.max=1;  x.delta=.1;
  real u(x,t) = x + t;
  real v = u(1,3);
}

  
  
