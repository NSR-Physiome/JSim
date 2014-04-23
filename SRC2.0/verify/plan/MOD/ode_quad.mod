// ODE quadratic growth

math main {
	realDomain t;
	t.min=0; t.max=3; t.delta=0.25;
	real u(t);
	when (t=t.min) u=0;
	u:t = t;
}
