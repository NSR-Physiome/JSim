// ODE exp growth private variable

math main {
	realDomain t;
	t.min=0; t.max=3; t.delta=0.25;
	private real u(t);
	when (t=t.min) u=1;
	u:t = u/(t+1);
	real uf = u(t.max);
}
