// external time controls:  tests extern parm loading

math main {
	realDomain t;
	t.min=0; 
	extern t.max; 
	extern t.delta;
	real u(t);
	when (t=t.min) u=1;
	u:t = u;
}
