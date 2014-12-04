JSim v1.1

import nsrunit;
unit conversion on;

math main {
	realDomain t sec;
	t.min=0; t.max=2; t.delta=1;

	// null to dimless test
  	real a0 = 2 sec;
  	real b0 = 3;
  	real c0 = a0 * b0;

	// power test
	real a1 = 16 m^4;
	real b1;
	b1^2 = a1;

	// absolute value test
	real a3 = 1 m^2;
	real b3 = 2;
	real c3 = a3 + abs(b3);

	// ODE test
	real a4 = 4 m;
	real b4 = 10 m/sec;
	real c4 = a4*b4;

	real h4(t);
	when (t=t.min) h4=1;
	h4:t = -c4;

	// min/sec -> dimensionless
	real a5 = 1 dimensionless;
	real b5 = 1 / (1 sec);
	real c5 = b5 * (1 min);

	// no dimensionless/sec
	real a6 = 1 dimensionless;
	real s6 = 1 sec;
	real b6 = a6 / s6;

	// united constants simplification
	real b7 = (10 m) / (2 sec);
	real c7 = b7 * (3 min);

}

