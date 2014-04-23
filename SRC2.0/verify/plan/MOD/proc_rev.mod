// procedure: reverse time series

source procedure reverse(u@t; v@t) {
	language="java";
	reentrant="true";
	maincode={{
	    RegularGridData t = (RegularGridData) u.grid(0);
	    for (int i=0; i<t.ct(); i++) {
		double uval = u.realVal(i);
		int vinx = t.ct()-i-1;
		v.set(vinx, uval);
	    }
	}};
}

math main {
	realDomain t;
	t.min=0; t.max=6; t.delta=2;
	real u(t) = t*t;
	real v(t);
	reverse(u@t, v@t); 
}

