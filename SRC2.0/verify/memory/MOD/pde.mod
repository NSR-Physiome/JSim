math main {
	realDomain t, x;
	t.min=0; t.max=3; t.delta=0.25;
	x.min=0; x.max=.6; x.delta=0.1;
	real u(t,x);
	when (t=t.min) u=0;
	when (x=x.min) u=t;
	when (x=x.max) u:x=0;
	u:t = u:x:x - u:x;
}
