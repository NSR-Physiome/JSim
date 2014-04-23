JSim v1.1
import MFAX;
import nsrunit;
unit conversion on;
MFAX example4 {
        Time t sec;
        t.min=0; t.max=10; t.delta=1;
        Chem A mM, B mM;
        Compartment C liter;
        C.vol = 10;
        MassBalReaction R(C, "A=2B");
	R.kf = 1;
	R.kb = 0;
	when (t=t.min) C.A = 1;
	C.B = t*(2 mol/liter/sec);
}

