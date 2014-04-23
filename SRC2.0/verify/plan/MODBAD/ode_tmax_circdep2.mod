// ODE required final value of state var

import nsrunit; unit conversion on;
math main {
	realDomain t;
	t.min=0; t.max=3; t.delta=0.25;
	real u(t), v(t);
	when (t=t.min) u=1;
	u:t = v(t.max);
	v = u;
}
