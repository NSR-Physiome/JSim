      SUBROUTINE gaudis(h, nh, dx, ndimen, area, sigma, xbar)
c
c Subroutine to generate a Gaussian distribution.
c
c From:    National Simulation Resource Facility
c          Center for Bioengineering (WD-12)
c          University of Washington
c          Seattle, WA 98195
c
c          Dr J. B. Bassingthwaighte, Director
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
c         This subroutine generates a Gaussian distribution.  Unless
c     limited to fewer points by the parameter ndimen, it generates
c     points from x = 0.0 to x = xbar + 4*sigma.
c         The first value calculated is that for the array point 
c     nearest to the specified mean using the formula:
c
c     h(x) = 1/sqrt(2*PI*sigma) * exp( -(xzero-xbar)**2 / 2*sigma**2 )
c
c     where xzero is the x value of the matrix point closest to xbar.
c     Calculation of the rest of the distribution proceeds "outward"
c     from xzero using the recursive formula:
c
c     h(x+dx) = h(x) * exp( -x*dx/sigma**2 ) * exp( -dx/2*sigma**2 )
c
c LIMITATIONS:
c     1.  Note that these distributions are computed pointwise, and
c         thus one may get some absurd results if the time step is
c         too large in comparison with the sigma.  
c     2.  The first point is at zero time, in accordance with what an
c         interpolation routine like FINTER would want to see.
c     3.  For a tau < .25 *dx, then a spike at the point closest to
c         the mean of the curve will be generated.
c     4.  If sigma or xbar <= 0, the distribution is not generated,
c         and nh is set to -1.
c
c CALL AND PARAMETER TYPES:
c
c     INTEGER nh, ndimen
c     REAL    h(ndimen), dx, area, sigma, xbar
c     CALL gaudis(h, nh, dx, ndimen, area, sigma, xbar)
c
c INPUTS:
c     dx     = The stepsize of independent variable (i.e. x-axis).
c     ndimen = The maximum number of points to be computed.
c     area   = The desired area under the Gaussian curve. (Usually 1.0)
c     sigma  = The standard deviation of the distribution.
c     xbar   = The mean value of the distribution.
c
c OUTPUTS:
c     h      = The array containing the Gaussian distribution.
c              Must be dimensioned at least (ndimen) in the mainline.
c     nh     = number of points returned in array h, or -1 if an error 
c              was detected and the distribution was not generated.
c
c SUBROUTINES CALLED:
c     NONE
c
c HISTORY:
c     Modified:
c         11/11/87  B. Van Steenwyk
c             Cleaned up (much)
c         03/25/88  A. Joseph 
c             Eliminated dbl. prec. rtne, corrected doc.
c
c --------------------------------------------------------------------
c
      INTEGER ndimen, nh, kstart, i
      REAL  h(ndimen), dx, area, sigma, xbar
      DOUBLE PRECISION  cumula, steper, sumold, sumnew
      DOUBLE PRECISION  factr1, factr2, x, asum
c
      CHARACTER*64 SID1, SID2
c
c Source Code Control Data
c
      DATA sid1/'@(#)gaudis.f	1.6 created 01/02/90 15:07:57.'/
      DATA sid2/'@(#) retrieved 03/31/00 22:20:39.'/
c
c -------------------------------------------------------------------
c
c  0.	Check for spike output and illegal parameter values
c
      nh = -1
      IF ((sigma.LE.0.).OR.(xbar.LE.0.))  RETURN
      IF (sigma.LT.(.25*dx))  THEN
	  nh = xbar/dx +1.5
	  IF (nh.LE.(ndimen-1))  THEN
	      DO 5 i=1, nh
		  h(i) = 0.0
    5         CONTINUE
	      IF (area.GT.0.)  THEN
		  h(nh) = area/dx
              ELSE
		  h(nh) = 1./dx
              END IF
	      nh = nh +1
	      h(nh) = 0.0
	  ELSE
	      nh = -1
	  END IF
	  RETURN
      END IF
c
c   I.  Compute the scaling factors common to all calculations.
c       Note:  1/sqrt(2*PI) = 0.39894228
c
      factr1 =  0.39894228/sigma
      factr2 = -0.50000000/(sigma*sigma)
c
c  II.  First compute the point closest to the center and backwards to the 
c       first point.
c
      kstart = xbar/dx + 1
c
      IF (kstart.GT.0) THEN
c
c     A.  Calculate the constants for this series.
c
          IF (kstart .GT. ndimen)  kstart = ndimen
          x = (kstart-1)*dx -xbar
          cumula = dexp(factr2*x*x) * factr1
          steper = dexp(factr2*dx*dx)
          sumold = dexp(-factr2*x*dx)
c
c     B.  Calculate the points in this series.
c
          DO 20 i=kstart, 1, -1
              h(i)     = cumula
              sumnew   = sumold*steper
              cumula   = cumula*sumold*sumnew
              sumold   = sumnew
   20     CONTINUE
c
c     C.  Set nh in case Part III is not executed.
c
	  nh = kstart
      END IF
c
c III.	Compute the points above the center point.
c
      kstart = kstart+1
c
      IF (kstart .LE. ndimen) THEN
c
c     A.  Calculate the constants for this series.
c
          IF (kstart .LT. 1)  kstart = 1
          x      = (kstart-1)*dx - xbar
          nh     = kstart + 4.*sigma/dx
          IF (nh .GT. ndimen)  nh = ndimen
          cumula = dexp(factr2*x*x) * factr1
          steper = dexp(factr2*dx*dx)
          sumold = dexp(factr2*x*dx)
c
c     B.  Calculate the points for this series.
c
          
          DO 30 i=kstart, nh
              h(i)   = cumula
              sumnew = sumold*steper
              cumula = cumula*sumold*sumnew
              sumold = sumnew
   30     CONTINUE
      END IF
c
c  IV. Normalize the area of the distribution to that set in the call.
c
      IF (area .GT. 0.0) THEN
          asum = 0.0
          DO 40 i=1,nh
              asum = asum + h(i)
   40     CONTINUE
          IF (asum .GT. 0.0) THEN
              asum = area/(asum*dx)
              DO 41 i=1,nh
                  h(i) = h(i) * asum
   41         CONTINUE
          END IF
      END IF
c
      RETURN
      END
