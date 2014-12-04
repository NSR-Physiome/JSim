JSim v1.1

source real function square(a) {
	language = "C";
	reentrant="true";
	maincode={{
	    double aval = a->realVal[0];
	    JSIM_RETURN(aval*aval);
	}};
}

math main {
	realDomain t;
	t.min=0; t.max=4; t.delta=1;
	real u(t) = square(t+1);
}

