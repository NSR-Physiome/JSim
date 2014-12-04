JSim v1.1 

import MFAX;

MFAX test1 { 
  Time t;
  Chem S, P;
  Compartment r1, r2, r3;
  FlowSource f1(r1);
  Flow f2(r1,r3);
  FlowSink f3(r3);
  Inject inj(f1, S);
  Membrane m(r1, r2);
  TransportPS trS1(m, S), trP1(m, P);
  MassBalReaction reac(r2, "S=P");

  t.min = 0;
  t.max = 10;
  t.delta = 0.1;
  r1.vol = 0.05;
  r2.vol = 0.3;
  r3.vol = 0.1;
  reac.kf = 0.02;
  reac.kb = 0;
  trS1.PS = 0.01;
  trP1.PS = 0.03;
  f1.flow = 0.02;

  extern inj.flux;
  private f2.flow, f3.flow;
}

