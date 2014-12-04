// mixed mass-balance and fast reactions
import MFAX;
import nsrunit;
unit conversion on;
MFAX example7 {
        Time t sec;
        t.min=0; t.max=10; t.delta=1;
        Chem X mM, Y mM, Z mM;
        Compartment C liter;
        C.vol = 10;
        MassBalReaction R1(C, "X=2Y");
	R1.kf = 1;
	R1.kb = 0;
	FastReaction R2(C, "Y=Z");
	R2.k = 1;
	when (t=t.min) C.X = 1;
	when (t=t.min) C.Y = 0;
	when (t=t.min) C.Z = 0;
}

