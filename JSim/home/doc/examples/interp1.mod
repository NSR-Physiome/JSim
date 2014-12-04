// variable interpolation
math interp1 {
  realDomain t; 
  t.min=0; t.max=3; t.delta=0.5;
  extern real u(t);
  real v(t);
  v = u^2;
}
