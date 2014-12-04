JSim v1.1

native real function square(a) {
	language = "C";
	library = "EBCommon";
	name = "csquare";
	reentrant="true";
}

math main {
	realDomain t;
	t.min=0; t.max=4; t.delta=1;
	real u(t) = square(t+1);
}

