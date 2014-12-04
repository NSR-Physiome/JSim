// 2-slab diffusion

import nsrunit;
unit conversion on;

math Two_Slab_Diff{
	real					// MODEL PARAMETERS
	D1 = 0.1 cm^2/sec,			// Diffusion coeff. in region 1
	D2 = 0.01 cm^2/sec,			// Diffusion coeff. in region 2
	C1_0 = 10 mM,				// Left hand boundary condition
	C2_0 = 10 mM,				// Right hand boundary condition
	delx = 0.001 cm;			// Delta x for approximate flux		

	realDomain 				// DOMAIN VARIABLES
	t sec; t.min=0; 			// Time domain
	t.max=50.0; t.delta=0.05;
    	realDomain 
	x cm; x.min=0;				// Normalized spatial domain
	x.max=1; x.delta=0.01;
	real					// MODEL VARIABLES
	C1(x,t) mM,				// Concentration in region 1				
	C2(x,t) mM,				// Concentration in region 2
	C1del(t) mM,				// C1 delx from interface
	C1int(t) mM,				// C1 at interface
	C2del(t) mM,				// C2 delx from interface
	C2int(t) mM,				// C2 at interface
	Q1mid(t) mM*cm/sec,			// Approx flux Q1 at interface
	Q2mid(t) mM*cm/sec;			// Approx flux Q2 at interface

	when (t=t.min) {
		C1 = if(x=x.min) C1_0 else 0;
		C2 = if(x=x.min) C2_0 else 0;
		}
	when (x=x.min) {
		C1 = C1_0;
		C2 = C2_0;
		}
	when (x=x.max) {
		C1:x = -Q2mid / D1;
		C2 = C1int;
		}

	C1del = C1(x.max-delx,t);
	C1int = C1(x.max,t);
	C2del = C2(x.max-delx,t);
	C2int = C2(x.max,t);
	
	Q1mid = D1 * (C1del - C1int)/delx;
	Q2mid = D2 * (C2int - C2del)/delx;

	C1:t = D1*(C1:x:x);
	C2:t = D2*(C2:x:x);
}
