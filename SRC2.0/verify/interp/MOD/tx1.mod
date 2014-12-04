JSim v1.1

math main {
	realDomain t;
	t.min=0; t.max=5; t.delta=1;
	realDomain x;
	x.min=-2; x.max=2; x.delta=0.5;
	real u(t) = t^2;
}



