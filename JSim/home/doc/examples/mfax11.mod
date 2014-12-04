// flow source, flow and flow sink
import MFAX;
import nsrunit;
unit conversion on;
MFAX example11 {
        Time t sec;
        t.min=0; t.max=10; t.delta=1;
        Chem A mM;
        Compartment C1 liter, C2 liter;
        C1.vol = 10; C2.vol = 5;
	Production Q1(C1, A);
	Q1.flux = 1;	
	FlowSource F1(C1) liter/sec;
	F1.flow = 1;
	Flow F2(C1, C2);
	FlowSink F3(C2);
}

