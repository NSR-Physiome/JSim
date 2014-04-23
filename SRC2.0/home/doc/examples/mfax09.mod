// consumption and production
import MFAX;
import nsrunit;
unit conversion on;
MFAX example9 {
        Time t sec;
        t.min=0; t.max=10; t.delta=1;
        Chem A mM;
        Compartment C1 liter, C2 liter;
        C1.vol = 10; C2.vol = 5;
	when (t=t.min) C1.A = 0;
	when (t=t.min) C2.A = 1;
	real t0 = 1 sec;
	real R = 1 mole/sec;
	Production Q1(C1, A);
	Q1.flux = R*exp(-t/t0);
	Consumption Q2(C2, A);
	Q2.flux = R*exp(-t/t0);
}

