JSim v1.1

math main {
	realDomain t;
	t.min=0; t.max=6; t.delta=1;
	realDomain x;
	x.min=1; x.max=2; x.delta=0.25;
	realDomain y;
	y.min=-2; y.max=2; y.delta=0.5;
	real u(x,t) = t + x;
}



