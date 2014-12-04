JSim v1.1

source procedure fill(a; b@x) {
	language="java";
	reentrant="true";
	maincode={{
	    RegularGridData x = (RegularGridData) b.grid(0);
	    double aval = a.realVal();
	    for (int i=0; i<x.ct(); i++) {
		b.set(i, aval+i);
	    }
	}};
}

math main {
	realDomain t;
	t.min=0; t.max=6; t.delta=2;
	real u = 10;
	real v(t);
	fill(u, v@t); 
}

