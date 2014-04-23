JSim v1.1

// PDE w/ extra dim

math p1 {
	realDomain t;
	t.min=0; t.max=3; t.delta=0.25;
	realDomain x;
	x.min=0; x.max=1; x.delta=0.1;
	realDomain y;
	y.min=0; y.max=1; y.delta=0.25;
	real u(t,x,y);
	when (t=t.min) u=x^2+y^2;
	when (x=x.min) u=t+y^2;
	when (x=x.max) u:x=0;
	u:t = u:x:x - u:x - u;
}
