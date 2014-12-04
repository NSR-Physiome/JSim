// dynamic linear 2-degree Fzero solver

math ax03 {
	realDomain t;
	t.min=0; t.max=5; t.delta=1;
	real a(t), b(t);
	a + b = (t+1)^2;
	a - b = (t-1)^2;
	real aexact(t) = t^2+1;
	real bexact(t) = 2*t;
}
