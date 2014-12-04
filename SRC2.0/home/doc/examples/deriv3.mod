// derivative operator applied to complex expression
math deriv3 {     // same result as deriv1 above
  realDomain t;
  t.min=0; t.max=10; t.delta=1;
  real v(t) = (3*sin(t)^2):t;        // v = 6*sin(t)*cos(t)
}
