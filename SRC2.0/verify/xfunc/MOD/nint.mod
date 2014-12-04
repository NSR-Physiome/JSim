JSim v1.1

native real function integral1(a@t) {
	language = "C";
	library = "EBCommon";
	name = "cintegral";
	reentrant="true";
}

math main {
	realDomain t;
	t.min=0; t.max=4; t.delta=1;
	real u(t) = t^2;
	real v = integral1(u@t);
}

