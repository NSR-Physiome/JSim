import nsrunit;
unit conversion off;

math main {
  realDomain t sec;
  t.min=0; t.max=60; t.delta=10;
  real a = 1 cm/sec;
  real b = 30 cm/min;
  extern real Cin(t) min;
  real Cout(t) m;
  Cout = (a+b)*Cin;
  real d = 1 cm;
}
