import nsrunit;
unit conversion on;

math main {
	realDomain t sec;
	t.min=0; t.max=5; t.delta=1;
	real u(t) = t^2 * (1 m/sec^2);
	real v = sum(t=t.min to t.max, u);
}

