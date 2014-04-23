math main {
	realDomain t;
	t.min=0; t.max=5; t.delta=1;
	realDomain x;
	x.min=-1; x.max=1; x.delta=.5;
	real u(t,x) = t^2 + x^2;
	real v(x) = integral(t=t.min to t.max, u);
}

