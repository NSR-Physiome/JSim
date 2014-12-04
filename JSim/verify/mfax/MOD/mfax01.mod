JSim v1.1
import MFAX;
MFAX example1 {
        Time t;
        t.min=0; t.max=10; t.delta=1;
        Chem A, B;
        Compartment C;
        C.vol = 10;
        MassBalReaction R(C, "A=2B");
	R.kf = 1;
	R.kb = 0;
}

