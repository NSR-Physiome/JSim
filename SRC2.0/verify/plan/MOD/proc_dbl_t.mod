// procedure: 0d args over loop

source procedure double(a; b) {
	language="java";
	reentrant="true";
	maincode={{
	    double aval = a.realVal();
	    b.set(aval*2);
	}};
}

math main {
	realDomain t;
	t.min=0; t.max=3; t.delta=1;
	real u(t) = t+1;
	real v(t);
	double(u, v);
}

