// bad procedure call input, fill time series with single value + double scalar value

source procedure fill(a; a2, v@t) {
	language="java";
	reentrant="true";
	maincode={{
	    double aval = a.realVal();
            a2.set(aval*2);
	    RegularGridData t = (RegularGridData) v.grid(0);
	    for (int i=0; i<t.ct(); i++) 
		v.set(i, aval);
	}};
}

math main {
	realDomain t;
	t.min=0; t.max=6; t.delta=2;
	real a(t) = 12;
	real b, v(t);
	fill(a, b, v@t); 
}

