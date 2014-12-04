// procedure: reverse time series, return sum

source procedure reversesum(u@t; v@t, vtot) {
	language="java";
	reentrant="true";
	maincode={{
	    RegularGridData t = (RegularGridData) u.grid(0);
	    double tot = 0;
	    for (int i=0; i<t.ct(); i++) {
		double uval = u.realVal(i);
		int vinx = t.ct()-i-1;
		v.set(vinx, uval);
		tot += uval;
	    }
	    vtot.set(tot);
	}};
}

math main {
	realDomain t;
	t.min=0; t.max=6; t.delta=2;
	real u(t) = t*t;
	real v(t), vtot;
	reversesum(u@t, v@t, vtot); 
}

