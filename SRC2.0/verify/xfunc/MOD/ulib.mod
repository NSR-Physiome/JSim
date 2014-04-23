JSim v1.1

import libsq;

math main {
	realDomain t;
	t.min=0; t.max=4; t.delta=1;
	real u(t), v(t);
	u = 2*t;
	v = square(u+1);
}

