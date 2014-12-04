// recirculating flow with junction

import MFAX;

MFAX recirc { 
  Time t;
  Chem A;
  Compartment r1, r2;
  FlowJunc j1, j2;
  FlowSource f1(j2, j1);
  Inject inj(f1, A);
  Flow f2(j1, r1);
  Flow f3(j1, r2);
  Flow f4(r1, j2);
  Flow f5(r2, j2);
  FlowSink f6(j2);

  t.min = 0;
  t.max = 10;
  t.delta = 0.1;
  r1.vol = 0.5;
  r2.vol = 0.2;
  f1.flow = 0.7;

  extern inj.flux;
}

