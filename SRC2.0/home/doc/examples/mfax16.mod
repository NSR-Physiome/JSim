// production, reaction with flow
import MFAX;
import nsrunit;
unit conversion on;
MFAX example16 {
        Time t sec;
        t.min=0; t.max=10; t.delta=1;
        Chem A mM, B mM;
        Compartment C1 liter, C2 liter;
        C1.vol = 10; C2.vol = 5;
	Production Q1(C1, A);
	Q1.flux = 1;	
	MassBalReaction Q2(C2, "A=B");
	Q2.kf = 1; Q2.kb = 1;
	FlowSource F1(C1) liter/sec;
	F1.flow = 1;
	Flow F2(C1, C2);
	FlowSink F3(C2);
	private C1.B;
}

