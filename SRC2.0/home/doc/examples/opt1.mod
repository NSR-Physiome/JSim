// Optimization tutorial model
// this model shows exponential decay given
//   and amplitude (at time 0) and a decay rate

math main {
	realDomain t;
	t.min=0; t.max=3; t.delta=0.1;
	real amp=1;
	real decay=1;
	real u(t) = amp*exp(-t*decay);
	real v(t) = amp-u;
}
