// simple MFAX reaction, without units
import MFAX;
MFAX example1 {
  Time t;
  t.min=0; t.max=10; t.delta=0.5;
  Chem A, B;  // this is just a comment
  Compartment C;
  C.vol = 10;
  MassBalReaction R(C, "2A=B");
  R.kf = 1;
  R.kb = 0.5;
}
