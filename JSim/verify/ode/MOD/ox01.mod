// simplest ODE

math ox01 {
	realDomain t;
	t.min=0; t.max=3; t.delta=0.25;
	real u(t);
	when (t=t.min) u=1;
	u:t = -u;
	real uexact(t) = exp(-t);
}
