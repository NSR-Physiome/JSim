JSim v1.1

native real function square(a) {
	language = "C";	
	library = "EBCommon"; name = "csquare";
	reentrant="true";
}

native real function integral1(a@t) {
	language = "C"; 
	library = "EBCommon"; name = "cintegral";
	reentrant="true";
}

native procedure reverse(a@t; b@t) {
	language = "C";	
	library = "EBCommon"; name = "crev";
	reentrant="true";
}

math main {
	realDomain t;
	t.min=0; t.max=4; t.delta=1;
	real u(t) = square(t+1);
	real v = integral1(u@t);
	real w(t);
	reverse(u@t, w@t);
}

