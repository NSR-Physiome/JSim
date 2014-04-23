math main {
	realDomain t;
	t.min=0; t.max=6; t.delta=1;
	real u(t) = t^2;
	real v(t) = integral(t=t.min to t-1, u);
}

