JSim v1.1

source procedure xsinit(t; v) {
	language="java";
	reentrant="true";
	maincode={{
	    v.set(t.realVal() + 1);
	}};
}

math main {
	realDomain t;
	t.min=0; t.max=6; t.delta=2;
	real v;
	when (t=t.min) xsinit(t, v);
}

