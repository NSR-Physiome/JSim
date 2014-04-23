// ODE , create IC parm

import nsrunit; unit conversion on;
math main {
	realDomain t sec;
	t.min=0; t.max=3; t.delta=0.25;
	real u(t) m^2;
	when (t=t.min) u=1;
	u:t = u/(t+1);
}
