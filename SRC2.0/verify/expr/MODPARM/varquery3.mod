// varquery via -u "u(3-t)"

math main {
	realDomain t;
	t.min=0; t.max=3; t.delta=0.25;
	real u(t);
	when (t=t.min) u=1;
	u:t = u;
}
