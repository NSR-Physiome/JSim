// ODE with extra domain

math main {
	realDomain t, x;
	t.min=0; t.max=3; t.delta=0.25;
	x.min=0; x.max=1; x.delta=0.1;
	real a(t) = t+1;
	real u(t,x);
	when (t=t.min) u=x;
	u:t = a + u;
}
