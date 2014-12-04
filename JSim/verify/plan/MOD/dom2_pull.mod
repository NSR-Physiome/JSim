// 2 domains require pull

math main {
  realDomain x,y;
  x.min=0; x.max=1; x.delta=1;
  y.min=0; y.max=1; y.delta=1;
  
  real a(x) = x;
  real af = a(x.max);
  real b(y) = y + af;
  real bf = b(y.max);
  real c(x) = x + bf;
}

 
