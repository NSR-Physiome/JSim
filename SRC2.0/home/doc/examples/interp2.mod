// bullet-proof interpolation
math interp2 {
  realDomain t;
  t.min=0; t.max=3; t.delta=0.5;
  private t.min;
  extern real u(t);
  real v(t);
  v = if (t<1) 0 else u(t-1)^2;
}
