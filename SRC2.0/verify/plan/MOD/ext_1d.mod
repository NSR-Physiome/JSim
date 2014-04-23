// 1D extern

math main {
  realDomain t;
  t.min=0; t.max=4; t.delta=1;
  extern real Cin(t);
  real Cout(t) = Cin;
}
