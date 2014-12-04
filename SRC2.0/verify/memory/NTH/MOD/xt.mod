math main {
  realDomain x, y;
  x.min=0; x.max=1; x.delta=.1;
  y.min=0; y.max=10; y.delta=2.5;
  
  real u(x,y) = x+y;
}
