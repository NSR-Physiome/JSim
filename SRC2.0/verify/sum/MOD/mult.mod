math main {
	realDomain t;
	t.min=0; t.max=5; t.delta=1;
	real u(t) = t^2;
	real v = sum(t=t.min to t.max, u);
	real v1 = sum(t=t.min to t.max, u+1);
	real v2 = sum(t=t.min to t.max, u+2);
}

