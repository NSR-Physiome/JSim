// variable query basic

math ax07 {
	realDomain t;
	t.min=0; t.max=10; t.delta=1;
	real u(t) = t^2;
	real v(t) = if (t<5) t else u(t-5);
}
