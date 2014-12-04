// VFC query of ODE: all possible queries

math main {
  realDomain t;
  t.min=0; t.max=1; t.delta=.1;
  real d = .1;
  real u(t);
  when (t=t.min) u=1;
  u:t = -u;

  real wmin(t) = u(t.min);
  real wdelay1(t) = if (t<d) u(t.min) else u(t-d);
  real wdelay2(t) = u(if (t<d) t.min else t-d);
  real wmax(t) = u(t.max);
  real wxexpr(t) = u(t^2);
  real wexpr(t) = u(d);
}
