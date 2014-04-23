// flux-based transport
import MFAX;
import nsrunit;
unit conversion on;
MFAX example10f {
  Time t sec;
  t.min=0; t.max=10; t.delta=1;
  Chem A molar;
  Compartment C1 liter, C2 liter;
  C1.vol = 10; C2.vol = 5;
  Production Q(C1, A);
  Q.flux = (1 mole/sec);
  Membrane M(C1, C2);
  TransportFlux T(M, A);
  T.flux = 1;  // flux has units mole/sec
}
