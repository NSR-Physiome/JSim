// ODE required final value of state var, using when(t=t.max)

import nsrunit; unit conversion on;
math main {
	realDomain t;
	t.min=0; t.max=3; t.delta=0.25;
	real u(t), uf;
	when (t=t.min) u=1;
	u:t = uf;
	when (t=t.max) uf = u;
}
