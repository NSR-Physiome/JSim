JSim v1.1
import MFAX;
import nsrunit;
unit conversion on;
MFAX example6 {
        Time t sec;
        t.min=0; t.max=10; t.delta=1;
        Chem A mM, B mM;
        Compartment C liter;
        C.vol = 10;
        FluxReaction R(C, "A=2B");
	real Vmax = 5 mole/(liter*sec);
	real Km = 1 mole/liter;
	R.flux = Vmax*C.A/(Km + C.A)*C.vol;
	when (t=t.min) C.A = 1;
	when (t=t.min) C.B = 3;
}

