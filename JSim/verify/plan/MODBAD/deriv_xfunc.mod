// symbolic deriv: xfunc not supported

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
  t.min=0; t.max=1; t.delta=0.1;
  real a(t) = square(t@t);
  real b(t) = a:t;
}

