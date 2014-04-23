// ODE w/ extra dimension

math ox01 {
	realDomain t;
	t.min=0; t.max=5; t.delta=1;
	realDomain x;
	x.min=0; x.max=1; x.delta=0.25;
	real u(t,x);
	when (t=t.min) u=x;
	u:t = 2*t;
	real uexact(t,x) = t^2+x;
}
