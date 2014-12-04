// 2 ODEs w/ delay line

// Note: accuracy of v depends on t.delta

math ox01 {
	realDomain t;
	t.min=0; t.max=3; t.delta=0.05;
	real u(t), v(t);
	when (t=t.min) { 
	    u=1;
	    v=exp(1);
	}
	u:t = -u;
	v:t = if (t<1) -exp(1-t) else -u(t-1); 
	real uexact(t) = exp(-t);
	real vexact(t) = exp(1-t);
}
