JSim v1.1

import nsrunit;
unit conversion on;

math main {
	realDomain t;
	t.min=0; t.max=5; t.delta=1;
	realState u(t) km/sec;
	real v = 2 km;
	when (t=t.min) u=1;
	event (t>0) u = v;
}
