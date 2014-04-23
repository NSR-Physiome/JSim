unit conversion on;
import nsrunit;
math main {
  realDomain t sec;
  t.min=1; t.max=5; t.delta=1;
  real a(t) m/sec;
  real c = 1 m;
  a = c/t;
  real b = integral(a@t);
}
