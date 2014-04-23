// vfc of algebraic var:  EXPR (forces pull)

math main {
  realDomain t;
  t.min=0; t.max=1; t.delta=.1;
  real a = .1;
  real u(t) = t^2;
  real w(t) = u(a);
}
