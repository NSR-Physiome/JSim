// 2 co-blocked ODEs with extra dim, different orders

math ox02 {
	realDomain t;
	t.min=0; t.max=2; t.delta=1;
	realDomain x;
	x.min=1; x.max=2; x.delta=0.25;
	real u(t,x), v(x,t);
	when (t=t.min) u=2;
	when (t=t.min) v=0;
	u:t = x*v;
	v:t = x*u;
	real uexact(t,x) = exp(x*t) + exp(-x*t);
	real vexact(x,t) = exp(x*t) - exp(-x*t);
}

