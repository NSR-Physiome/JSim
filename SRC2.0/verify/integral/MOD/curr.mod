math main {
	realDomain t;
	t.min=0; t.max=5; t.delta=1;
	real u(t) = t^2;
	real v(t) = integral(t=t.min to t, u);
}

