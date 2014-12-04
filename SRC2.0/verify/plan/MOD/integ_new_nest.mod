// new form integral: nested

math main {
	realDomain t, x;
	t.min=1; t.max=3; t.delta=1;
	x.min=1; x.max=2; x.delta=1;
	real u(t,x) = t+x;
	real w = integral(t=t.min to t.max, integral(x=x.min to x.max, u));
}

