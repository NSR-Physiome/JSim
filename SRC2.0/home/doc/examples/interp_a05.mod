// interpolating extern data source
math a05 {
  realDomain t, x;
  t.min=1; t.max=5; t.delta=1;
  x.min=0; x.max=1; x.delta=0.01;
  extern real f(x);
  real g(t) = f(1/t);
}
