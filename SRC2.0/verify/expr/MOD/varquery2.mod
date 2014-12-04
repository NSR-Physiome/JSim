// varquery data interp

math a05 {
	realDomain t, x;
	t.min=1; t.max=5; t.delta=1;
	x.min=0; x.max=1; x.delta=0.01;
	real f(x) = x^2;
	real u(t) = f(1/t);
	real uexact(t) = 1/t^2;
}
