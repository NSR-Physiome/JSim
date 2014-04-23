JSim v1.1
import MFAX;
import nsrunit;
unit conversion on;
MFAX example8 {
        Time t sec;
        t.min=0; t.max=10; t.delta=1;
        Chem X mM, Y mM;
        Compartment C liter;
        C.vol = 10;
 	FastReaction R(C, "X=2Y");
	R.k = 1;
	real k = 2 mole/liter/sec;
	C.X = k*t;
	C.Y = 2*k*t;
}

