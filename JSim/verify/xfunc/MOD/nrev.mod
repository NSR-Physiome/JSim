JSim v1.1

native procedure reverse(a@t; b@t) {
	language = "C";
	library = "EBCommon";
	name = "crev";
	reentrant="true";
}

math main {
	realDomain t;
	t.min=0; t.max=4; t.delta=1;
	real u(t) = t^2;
	real w(t);
	reverse(u@t, w@t);
}
