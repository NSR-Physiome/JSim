// introductory algebraic model
math main {
  realDomain t;
  t.min=0; t.max=2*PI; t.delta=0.1;
  real amp = 1;
  real phase = 0;
  real u(t), v(t), w(t);
  u = amp*sin(t-phase);
  v = amp*cos(t-phase);
  w = u + v;
}
