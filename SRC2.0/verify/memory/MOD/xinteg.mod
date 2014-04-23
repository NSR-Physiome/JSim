math o1 {
	realDomain t, x;
	t.min=0; t.max=7; t.delta=1;
	x.min=1; x.max=4; x.delta=1;
	real u(t,x);
	u = t^2 + x;
	real v(t) = integral(u@x);
}
