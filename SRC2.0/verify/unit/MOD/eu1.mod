JSim v1.1

import nsrunit;
unit conversion on;

math main {
	realDomain t sec;
	t.min=0; t.max=5; t.delta=1;
	real m = 1 m;
	real kg = 1 kg;
	realState g(t) g;
	when (t=t.min) g = 1;
	event (t>2) 
	    g = kg + 1;
}
