// ODE with n serial tanks

math main {
	int n = 3;
	realDomain t, x;
	t.min=0; t.max=3; t.delta=0.25;
	x.min=1; x.max=n; x.delta=1;
	real Cin(t) = 1-exp(-t);
	real u(t,x);
	real k(x) = x;
	when (t=t.min) u=0;
	u:t = -k*((if (x=x.min) Cin else u(t,x-1)) - u);
}
