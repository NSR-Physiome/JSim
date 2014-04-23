// examines linear interpolation accuraccy 
math interp6 {
  realDomain t;
  t.min=0; t.max=1; t.delta=0.25;
  private t.min, t.max;
  real u(t) = t^2;
  real v(t) = u(t^2);
  real w(t) = t^4;
}
