// linear implicit eqns of 2 variables
math implicit2 {
  realDomain t; t.min=0; t.max=10; t.delta=1;
  real a=4, b=5;
  real x(t), y(t);
  a*x + b*y = t;
  b*x - y = a;
}

