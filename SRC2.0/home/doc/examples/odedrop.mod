// ODE variable with event-driven discontinuity fudge
math main {
  realDomain t;
  t.min=0; t.max=10; t.delta=.1;
  realState Vdrop(t);
  real Vmax = 5;
  real V(t);
  when (t=t.min) {
    V=0;
    Vdrop=0;
  }
  event (Vdrop=0 and V>=Vmax) {
    Vdrop = V;
  }
  event (V<Vmax) {
    Vdrop = 0;
  }
  V:t = if (Vdrop>0) -Vdrop/t.delta else t;
}
