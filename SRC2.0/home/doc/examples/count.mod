// event to count voltages above threshhold
math count {
  realDomain t;
  t.min=0; t.max=10; t.delta=1;
  real V0 = 0.5;
  extern real V(t);
  intState ct(t);
  when (t=t.min) ct=0;
  event (V>V0) ct=ct+1;
}
