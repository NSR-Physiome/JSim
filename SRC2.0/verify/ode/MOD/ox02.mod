// 2 co-blocked ODEs

math ox02 {
	realDomain t;
	t.min=0; t.max=3; t.delta=0.25;
	real u(t), v(t);
	when (t=t.min) u=2;
	when (t=t.min) v=0;
	u:t = v;
	v:t = u;
	real uexact(t) = exp(t) + exp(-t);
	real vexact(t) = exp(t) - exp(-t);
}

