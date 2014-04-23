      REAL FUNCTION erf(x)  
c 
c Evaluate the standard error function.
c 
c From:    National Simulation Resource Facility
c          Center for Bioengineering (WD-12)
c          University of Washington
c          Seattle, WA  98195
c
c          Dr. J. B. Bassingthwaighte, Director
c
c
c Copyright (C) 1988, 1989, 1990 by National Simulation Resource
c Facility, Univ of WA. All Rights Reserved.
c Software may be copied so long as this copyright notice is included.
c 
c This software was developed with support from NIH grant RR-01243.
c Please cite this grant in any publication for which this software
c is used and send one reprint to the address given above.
c
c
c DESCRIPTION:
c   This routine computes the value of integral, 2/sqrt(pi) * int[0,x]
c (exp(-t*t)) dt.  The legal argument range, x in (-infinity, infinity),
c is reduced to [0, infinity) via the identity erf(-x) = -erf(x).  It 
c uses either rational approximation or series expansion to calculate
c the error function.  It approximates the error function by a rational
c when the magnitude of the argument, x, is greater than 0.1, otherwise 
c it approximates by a 3-term series.  This routine yields at least 5 
c significant digits for all range of the arguments.
c 
c  The rational approximation is
c	erf(x) = 1 - t/exp(x^2)*(a1 +t*(a2 +t*(a3 +t*(a4 +t*a5))))
c	  where t = 1/(1+p*x) and p = 0.3275911000.
c
c  The 3-term series expansion is
c       erf(x) = 1/sqrt(pi)* exp(-x^2) * (x + 2/3 * x^3 +4/15 *x^5).
c
c CALL AND PARAMETER TYPES:
c     real x, y
c     y = erf(x)
c
c INPUT:
c     x   = real, the argument for the error function. 
c
c OUTPUT:
c     erf = real, the function value of the error function. 
c
c REFERENCES:
c     Milton Abramowitz and Irene A. Stegun,  Handbook of
c     Mathematical Functions, Dover Publications, Inc., 
c     New York, 1966.
c
c HISTORY:
c     Written:   
c         08/20/87      Brett Van Steenwyk 
c     Modified:   
c         04/29/88      Joseph Chan           
c             added series expansion for calculating small arguments 
c             accurately, and added document and comments.
c
c ---------------------------------------------------------------------
c
        REAL    x, z, ax, ax2, ax3, a1, a2, a3, a4, a5
        REAL    scale, c1, c2, c3
        CHARACTER*64 sid1, sid2
	DATA	a1/.254829592/, a2/-.284496736/, a3/1.421413741/
	DATA	a4/-1.453152027/, a5/1.061405429/, scale/.3275911000/
	DATA    c1/1.1283791671/,c2/0.6666666666/,c3/0.2666666666/
c
c Source Code Control Data
c
        DATA sid1/'@(#)erf.f	1.4 created 01/02/90 14:56:33.'/
        DATA sid2/'@(#) retrieved 03/31/00 22:20:31.'/
c
c I.  Computes constant variables.
c
	ax=ABS(x)
	ax2= ax*ax
c
c II. Computes the error function.
c
	IF(ax.GT.0.1) THEN
c
c     A.  Approximates the error function by the rational function.
c
	   z   = 1./(1.+scale*ax)
	   erf = 1. - z*EXP(-ax2)*(a1 +z*(a2 +z*(a3 +z*(a4 +z*a5))))
	ELSE
c
c     B.  Approximates the error function by the 3-terms series 
c         expansion.
c
	   ax3= ax*ax2
	   erf = c1 * EXP(-ax2) *(ax + c2*ax3 + c3*ax3*ax2)
	ENDIF
c
c III. Assigns the error function for the negative argument
c
	IF (x.LT.0.)  erf = -erf
c
c  IV. returns to calling program
c
	RETURN
	END
