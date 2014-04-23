// ODE missing state eqn
math main {
  realDomain t;
  real u(t);
  when (t=t.min) u=1;
}
