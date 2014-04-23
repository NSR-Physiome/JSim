// seqloops error: ODE state eqn
math main {
  realDomain t, x;
  t.min=0; t.max=4; t.delta=1;
  x.min=0; x.max=1; x.delta=.1;
  real u(t);
  real a(x) = x^2;
  when (t=t.min) { u=1; u:t=-1; }
  u:t:t + u:t = u+a;
}
