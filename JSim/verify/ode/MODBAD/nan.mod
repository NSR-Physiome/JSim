// this model should cancel itself immediately 
//   with NaN message from Dopri5
// If long wait,  followed by message from Radau
//   then dopri5 NaN check is not working

math main {
	realDomain t; 
	t.min=0; t.max=3; t.delta=.1;
	real a(t) = 0;
	real b(t) = 0;
	real u(t);
	when (t=t.min) u=1;
	u:t = a/b;
}
