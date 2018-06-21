// radioactive decay
// A short description: look at the decay rate.
unit conversion on;

/* This the first comment */


unit
	// fundamental units
	gtg = fundamental;		// gigatongrams

import nsrunit;


/* This is another comment */

math main {
  realDomain t sec;   // Time domain
  t.min=0; t.max=4; t.delta=0.1;
  real rate = 1 1/sec;
  real u(t) kg;        // ODE variable declaration
 
  when (t=t.min) u=1;  // initial condition for u
  u:t = -rate*u;       // state equation for u
}

/* This model used to test exporting
   comments to SBML.

*/
