// ODE exp growth, IC not parm (calculated from another var)

math main {
	real a=1;
	realDomain t;
	t.min=0; t.max=3; t.delta=0.25;
	real u(t);
	when (t=t.min) u=a;
	u:t = u;
}
