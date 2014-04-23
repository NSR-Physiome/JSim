JSim v1.1

source real function square(a) {
	language="java";
	reentrant="true";
	maincode={{
	    double aval = a.realVal();
	    return aval*aval;	
	}};
}

math main {
	realDomain t;
	t.min=0; t.max=4; t.delta=1;
	real u(t), v(t);
	u = 2*t;
	v = square(u+1);
}

