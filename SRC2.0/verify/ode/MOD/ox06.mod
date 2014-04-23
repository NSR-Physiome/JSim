// ODE w/ aux var requires IC for proper planning
//	ODE solver must export() at tf as well as t0 for each step

math ox06 {
	realDomain t;
	t.min=0; t.max=1; t.delta=0.1;
	real w(t), u(t), v(t);
	when (t=t.min) u=1;
	w = -v;
	v = u - 3*exp(-2*t);
	u:t = v;

	real uexact(t);
	real vexact(t);
	real wexact(t);
	uexact = exp(-2*t);
	vexact = -2*exp(-2*t);
	wexact = 2*exp(-2*t);
}

