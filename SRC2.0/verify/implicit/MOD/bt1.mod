JSim v1.1

math main {
	realDomain t;
	t.min=0; t.max=6; t.delta=1;
	real x(t);
	x^2 = t;
	x ~= t/3;
	x >= 0;
	x <= t+5;
}
