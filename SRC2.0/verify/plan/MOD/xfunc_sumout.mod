// xfunc sums out domain

source real function mysum(u@t) {
	language="java";
	reentrant="true";
	maincode={{
	    RegularGridData t = (RegularGridData) u.grid(0);
	    double tot = 0;
	    for (int i=0; i<t.ct(); i++) 
		tot += u.realVal(i);
	    return tot;	
	}};
}

math main {
	realDomain t;
	t.min=0; t.max=6; t.delta=2;
	real u(t) = t*t;
	real u1(t) = u(6-t);
	real v = mysum(u(6-t)@t);
}

