JSim v1.1
import MFAX;
import nsrunit;
unit conversion on;
MFAX example4 {
        Time t sec;
        t.min=0; t.max=10; t.delta=1;
        Chem A mM, B mM, D mM;
        Compartment C liter;
        C.vol = 10;
        MassBalReaction R(C, "A=2B");
	R.kf = 1;
	R.kb = 0;
	when (t=t.min) C.A = 1;
	when (t=t.min) C.D = 0;
	C.D:t = (2 mol/liter/sec);
}

