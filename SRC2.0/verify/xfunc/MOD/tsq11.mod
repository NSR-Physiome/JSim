JSim v1.1

class real function square {
	class="EBsquare";
	reentrant="true";
}

math main {
	realDomain t;
	t.min=0; t.max=6; t.delta=2;
	real u(t), v(t);
	u = t+2;
	v = square(u+1);
}

