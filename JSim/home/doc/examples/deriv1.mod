// symbolic derivative
math deriv1 {
  realDomain t;
  t.min=0; t.max=10; t.delta=1;
  real u(t) = 3*sin(t)^2;
  real v(t) = u:t;      // v = 6*sin(t)*cos(t)
}
