// ODE for n parallel tanks

math main {
	int n = 3;
	realDomain t, x;
	t.min=0; t.max=3; t.delta=0.25;
	x.min=1; x.max=n; x.delta=1;
	real uIn(t) = t;
	real u(t,x);
	real k(x) = x;
	when (t=t.min) u=uIn;
	u:t = -k*u;
	real uOut(t) = sum(u@x);
}
