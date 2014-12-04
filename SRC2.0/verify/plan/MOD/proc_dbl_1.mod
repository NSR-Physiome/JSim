// procedure: 0d args called once

source procedure double(a; b) {
	language="java";
	reentrant="true";
	maincode={{
	    double aval = a.realVal();
	    b.set(aval*2);
	}};
}

math main {
	real u = 1;
	real v;
	double(u, v);
}

