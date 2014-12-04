// new form integral: 1 variable for all time

math main {
	realDomain t;
	t.min=0; t.max=6; t.delta=1;
	real u(t) = t*t;
	real w = integral(t=t.min to t.max, u);
}

