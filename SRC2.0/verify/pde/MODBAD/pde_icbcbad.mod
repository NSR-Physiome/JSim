// IC/LHBC conflict at run-time

math p1 {
	realDomain t;
	t.min=0; t.max=3; t.delta=0.25;
	realDomain x;
	x.min=0; x.max=1; x.delta=0.1;
	real u(t,x), v(t,x);
	when (t=t.min) { u=0; v=1; }
	when (x=x.min) { u=t; v = 2*t; }
	when (x=x.max) { u=t^2; v:x = 0; }
	u:t = u:x:x - u:x + u - v;
	v:t = v:x:x + v:x + v - u;
}
