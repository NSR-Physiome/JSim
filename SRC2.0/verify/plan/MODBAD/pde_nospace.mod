// PDE missing spatial domain

math main {
	realDomain t, x;
	t.min=0; t.max=10; t.ct=1;
	x.min=0; x.max=10; x.ct=1;
	real a(t);
	when (t=t.min) a=0;
	when (x=x.min) a=exp(t);
	when (x=x.max) a:x=0;
	a:t = a:x:x;
}
