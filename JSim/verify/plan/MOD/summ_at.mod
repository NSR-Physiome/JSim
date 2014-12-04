// old form summation: 1 variable

math main {
	realDomain t;
	t.min=0; t.max=6; t.delta=1;
	real u(t) = t*t;
	real w = sum(u@t);
}

