JSim v1.1

class procedure reverse {
	class="EBreverse";
	reentrant="true";
}

math main {
	realDomain t;
	t.min=0; t.max=6; t.delta=2;
	real u(t) = t*t;
	real v(t); 
	reverse(u@t, v@t); 
}

