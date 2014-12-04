// NaN protection test
// v is very close to t,  but not exact due to machine rounding
// without NaN protection, w(t) will contain NaNs for all
//   t such that v(t) > t

math main {
	realDomain t;
	t.min=0; t.max=2*PI; t.ct=101;
	real u(t) = t;
	real v(t) = sin(t)^2*t + cos(t)^2*t;
	real w(t) = u(v);
}
