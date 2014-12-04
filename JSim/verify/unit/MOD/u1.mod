JSim v1.1

import nsrunit;
unit conversion on;

math main {
	real a = 1 kg/m/sec;
	real b = 2 kg/(m/sec);
	real c = 3 kg/(m/sec)^2;
	real d = 4 m/sec;
	real e = a/b;
	real f = d^2;
	real g = 5 1/(m*sec); // 1.6.50 and older can't parse this
	real g1 = 5 1/(m*sec)^2; // 1.6.50 and older can't parse this
	real h = 6 kg/(m*sec);
	real i = 7 kg;
	real j = i/a;

	realDomain t sec;
	t.min=0; t.max=5; t.delta=1;
	realState s1(t), s2(t);
	when (t=t.min) {
	    s1 = (1 kg);
	    s2 = 1;
	}
	event (t>1) {
	    s1 = 2;
	    s2 = t+1;
	}

}
