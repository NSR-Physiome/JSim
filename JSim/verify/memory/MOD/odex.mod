// simplest ODE

math o1 {
	realDomain t, x;
	t.min=0; t.max=3; t.delta=0.25;
	x.min=1; x.max=4; x.delta=1;
	real u(t,x);
	when (t=t.min) u=x;
	u:t = u;
}
