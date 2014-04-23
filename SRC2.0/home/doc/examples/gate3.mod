// gate opening/closing drives model ODE
math main {
  realDomain t;
  t.min=0; t.max=10; t.delta=0.1;
  real V(t);
  real V1 = 10;
  real V0 = 5;
  intState open(t);
  when (t=t.min) {
    open = 0;
    V=1;
  }
  V:t = if (open>0) -V else V;
  event (open=0 and V>V1) open = 1;
  event (open>0 and V<V0) open = 0;
}
