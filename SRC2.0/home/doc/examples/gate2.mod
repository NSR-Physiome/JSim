// ensuring gate openings/closings occur alternately
math main {
  realDomain t;
  t.min=0; t.max=10; t.delta=0.1;
  extern real V(t);
  real V1 = 10;
  real V0 = 5;
  intState open(t);
  intState ct(t);
  realState tlast(t);
  when (t=t.min) {
    open = 0;
    ct = 0;
    tlast = -1;
  }
  event (open=0 and V>V1) {
    open = 1;
    ct = ct+1;
  }
  event (open>0 and V<V0) {
    open = 0;
    tlast = t;
  }
}
