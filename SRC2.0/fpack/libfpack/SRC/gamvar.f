      SUBROUTINE gamvar(h,nh,dx,frpeak,ndimen,area,alpha,beta,xa)
c
c Subroutine to generate a gamma variate probability density function.
c
c From:   National Simulation Resource Facility
c         Center for Bioengineering (WD-12)
c         University of Washington
c         Seattle, WA  98195
c
c         Dr. J. B. Bassingthwaighte, Director
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
c         This routine generates a gamma variate probability density
c     function as described by Thompson et al. (1964).  The equation
c     for the distribution, as given by Equation 25 in the Appendix of
c     the reference, is:
c
c                         (x-xa)**alpha * exp(-(x-xa)/beta)
c                  h(x) = --------------------------------
c                         beta**(alpha+1) * gamma(alpha+1)
c
c     The shaping parameters alpha and beta and the appearance point xa
c     (i.e. the location of the first non-zero point of the
c     distribution) determine the distribution's mean, dispersion
c     (RD=SD/mean), and skewness.  Given an appearance point, mean, and
c     dispersion, alpha and beta can be calculated:
c
c        alpha = ((mean-xa)/SD)**2 - 1.0  and  beta  = SD**2/(mean-xa)
c
c     The skewness is then fixed at 2.0/sqrt(alpha+1).  The peak is
c     located at xa+alpha*beta and, for a given curve area, the peak
c     value is:
c
c                           area * (alpha*beta/e)**alpha
c               h(peak) = --------------------------------
c                         beta**(alpha+1) * gamma(alpha+1)
c
c
c     mean transit time = ta +beta*(alpha+1)
c
c     rd = sqrt(alpha+1)/(  ta +beta*(alpha+1) )
c 
c LIMITATIONS:
c     1.  The routine calculates point values, rather than integrals
c         binned into intervals and averaged for those intervals. 
c         Thus, the distribution is significantly deformed even before
c         the spike condition discussed below is reached.
c     2.  The first point is always at x=0.  This is in accord with the
c         form expected by the linear interpolation routine FINTER.
c     3.  Input parameters are restricted to the ranges given below.
c         Violations result in a return with nh = -1 and no points
c         calculated.
c              0 < alpha < (rfactr range - 1)  varies with the 
c                                              machine limits
c              0 < beta  < 20
c              0 < xa             IMPLIES: xa < mean
c              0 < dx
c     4.  Some combinations of input parameters will cause a spike to
c         be generated at x = xa + beta*(alpha+1).  A spike will be
c         generated if:
c              beta*sqrt(alpha+1) < dx/4     IMPLIES: SD < dx/4
c     5.  Having the maximum dimension, ndimen, of the output array too
c         small can result in a significant portion of the tail of the
c         curve being cut off.  As a rule-of-thumb, to insure that less
c         than 1% of the curve is cut off, keep
c         ndimen .GE. (xa + beta*(alpha+1) + 4*beta*sqrt(alpha+1)) / dx
c         IMPLIES:  ndimen .GE. (mean + 4*SD) / dx
c
c REFERENCES:
c     H.K. Thompson, C.F. Starmer, R.E. Whalen, and H.D. McIntosh. 
c     Indicator transit time considered as a gamma variate.  Circ.
c     Res. 14:502-515, 1964.
c
c
c     Starmer, C. F., and D. O. Clark. Computer computations of 
c     cardiac output using the gamma function. J. Appl. Physiol.
c      28:219-220, 1970.
c
c
c CALL AND PARAMETER TYPES:
c     REAL h, dx, frpeak, area, alpha, beta, xa
c     INTEGER nh, ndimen
c     CALL gamvar(h, nh, dx, frpeak, ndimen, area, alpha, beta, xa)
c
c INPUTS:
c     dx      = the independent variable increment
c     frpeak  = the proportion of peak height below which the curve is
c               not to be calculated (usually set to 0.01)
c     ndimen  = the maximum number of points to be computed
c     area    = desired area under the curve (usually 1.0; if set
c               .LE. 0.0, the points will not be scaled)
c     alpha   = shaping parameter (dimensionless)
c     beta    = shaping parameter (same dimension as xa)
c     xa      = appearance point (arbitrary dimension)
c
c OUTPUTS:
c     h       = a real array containing the computed concentration
c               values
c     nh      = number of points computed in h (or -1 in event of
c               error)
c
c SUBROUTINES CALLED:
c     gamma = calculate gamma function
c
c HISTORY:
c     Modified:
c         09/30/87  B. Van Steenwyk 
c            1) Revised and updated
c         12/11/87  R. King 
c            1) New Header
c         01/14/88  A. Joseph 
c            1) Clarified limitation for ndimen
c         03/25/88  A. Joseph 
c            1) Updated doc.
c         02/08/90  S. Castaldi
c            1) Updated header.
c
c -------------------------------------------------------------------
c
      INTEGER ndimen, nh, i, istart
      REAL    h(ndimen), dx, frpeak, area, alpha, beta, xa
      REAL    alp, scalar, xprime, deltax, expon, expndx, hmax
      REAL    squit, sum, aa, gamma
      EXTERNAL gamma
c
      CHARACTER*64 SID1, SID2
c
c Source Code Control Data
c
      DATA sid1/'@(#)gamvar.f	1.8 created 07/31/92 08:03:15.'/
      DATA sid2/'@(#) retrieved 03/31/00 22:20:38.'/
c
c   . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
c
c 0.  Check for error conditions and spike
c
      nh      = -1
c
c   A.  Error conditions
c
      IF ((gamma(alpha+1) .EQ. 0.).OR.(alpha .LE. 0.).OR.(xa .LE. 0.)
     +    .OR.(beta .GT. 20.).OR.(beta .LE. 0.).OR.(dx .LE. 0.)) RETURN
c
c   B.  Spike generation
c
      IF ((.25*dx).GT.(beta*sqrt(1.+alpha)))  THEN
	  nh = (xa +beta*(alpha+1))/dx +1.5
	  IF (nh .GT. (ndimen-1))  THEN
	      nh = -1
	      RETURN
          END IF
	  DO 5 i=1, nh
	      h(i)  = 0.
    5	  CONTINUE
	  IF (area .GT. 0.)  THEN
	      h(nh) = area/dx
	  ELSE
	      h(nh) = 1./dx
	  END IF
	  nh    = nh +1
	  h(nh) = 0.
	  RETURN
      END IF
c
c I.  Set up
c
      alp     = alpha + 1
      IF (area .GT. 0.)  THEN
          scalar = area / (beta * gamma(alp))
      ELSE
          scalar = 1.0  / (beta * gamma(alp))
      END IF
      istart  = xa / dx +1.99999
      xprime  = ( (istart - 1) * dx - xa ) /beta
      deltax  = dx/beta
      expon   = exp(-xprime)
      expndx  = exp(-deltax)
      hmax      = 0.
c
      DO 10  i = 1, istart - 1
          h(i) = 0.
   10 CONTINUE
c
c II. Compute h(i) at each dx
c
      DO 25  i =istart, ndimen
          h(i)   = scalar * (xprime**alpha) * expon
	  expon  = expon * expndx
          xprime = xprime + deltax
c
c       A.  Check for maximum concentration and calculate squit
c
          IF (h(i) .GE. hmax)  THEN
              hmax    = h(i)
              squit = frpeak * hmax
c
c       B.  Stop calculation when h(i) falls below squit of peak
c
          ELSE  IF (h(i) .LE. squit)  THEN
	      nh    = i
	      GOTO 30 
          END IF
c
   25 CONTINUE
c
c III. Set nh
c
   30 CONTINUE
      IF (nh.EQ.-1)  nh = ndimen
c
c IV. Set area under the curve to the desired value
c
      IF (area .GT. 0.0)  THEN
          sum     = 0.0
          DO 40  i =1, nh
              sum  = sum + h(i)
   40     CONTINUE
          sum     = sum * dx
	  IF (sum .GT. 0.0)  THEN
	      aa  = area / sum
	  ELSE
	      nh  = -1
	      RETURN
	  END IF
          DO 45  i =1, nh
              h(i) = h(i) * aa
   45     CONTINUE
      END IF
c
      RETURN
      END
