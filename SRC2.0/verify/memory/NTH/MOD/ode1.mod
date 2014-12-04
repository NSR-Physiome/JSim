// simplest ODE

math o1 {
	realDomain t;
	t.min=0; t.max=3; t.delta=0.25;
	real v(t) = t^2;
	real u(t);
	when (t=t.min) u=v+1;
	u:t = -u;
}
