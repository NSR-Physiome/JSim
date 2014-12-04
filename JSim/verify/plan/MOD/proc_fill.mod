// procedure, fill time series with single value

source procedure fill(a; v@t) {
	language="java";
	reentrant="true";
	maincode={{
	    double aval = a.realVal();
	    RegularGridData t = (RegularGridData) v.grid(0);
	    for (int i=0; i<t.ct(); i++) 
		v.set(i, aval);
	}};
}

math main {
	realDomain t;
	t.min=0; t.max=6; t.delta=2;
	real a = 12;
	real v(t);
	fill(a, v@t); 
}

