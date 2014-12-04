// PDE for n parallel tanks

math main {
	int n = 3;
	realDomain t, x, i;
	t.min=0; t.max=3; t.delta=0.25;
  	x.min=0; x.max=1; x.delta=.05;
	i.min=1; i.max=n; i.delta=1;
	real uIn(t) = 1-exp(-t);
	real u(t,x,i);
	real k(x) = x;
	when (t=t.min) u=0;
	when (x=x.min) u=uIn;
	when (x=x.max) u:x = 0;
	u:t = k*u:x:x - u:x;
	real uOut(t,x) = sum(u@i);
}

