source real function slow(a, b) {
	language="java";
	reentrant="true";
        maincode={{
            double aval = a.realVal();
	    long bval = (long) b.realVal();
	    try { Thread.sleep(bval); }
		catch (Exception e) { }
            return aval;
        }};
}
 
math main {
	realDomain t;
	t.min=0; t.max=3; t.delta=0.25;
	real a = 1;
	real delay = 50;
	real u(t);
	when (t=t.min) u=a;
	u:t = slow(u, delay);
	real uf;
	when (t=t.max) uf = u;
}

