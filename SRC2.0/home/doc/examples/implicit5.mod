// bounded non-linear implicit eqn
math implicit5 {
  realDomain t;
  t.min=0; t.max=10; t.delta=1;
  real x(t);
  x^2 = t;
  x >= 0;
  x <= if (t<1) 1 else t;
}
