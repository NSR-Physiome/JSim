      SUBROUTINE ranwok(h,nh,dt,frpeak,ndimen,area,ta,xbar,tkappa) 
c
c Modified first traversal of a random walk probability density routine
c
c From:   National Simulation Resource Facility
c         Center for Bioengineering (WD-12)
c         University of Washington
c         Seattle, WA  98195
c
c         Dr. J. B. Bassingthwaighte, Director
c
c
c Copyright (C) 1989, 1990 by National Simulation Resource
c Facility, Univ of WA. All Rights Reserved.
c Software may be copied so long as this copyright notice is included.
c 
c This software was developed with support from NIH grant RR-01243.
c Please cite this grant in any publication for which this software
c is used and send one reprint to the address given above.
c
c
c DESCRIPTION:
c         This routine generates a set of numbers using a scaled first
c     traversal of a random walk with an offset (axis translation).
c     For an unscaled first traversal of a random walk with no offset,
c     the relative dispersion, rd, and the skewness, skewn, are not
c     independent parameters.  By including a scaling parameter, xbar,
c     and an offset (or appearance time), ta, the skewness, relative
c     dispersion and mean become independent parameters of the
c     probability distribution.
c
c         The statistical parameters for this distribution, mean
c     (tmean), relative dispersion(rd) and skewness (skewn) are given
c     by:
c
c       tmean = ta + xbar.
c
c       rd = xbar*tkappa/ (  (ta+xbar)*SQRT(2.)  ).
c
c       skewn = 3.0*tkappa/SQRT(2.).
c
c     Given tmean, rd, and skewness, ta, xbar, and tkappa can be
c     calculated:
c
c       tkappa = skewn*SQRT(2.)/3.
c       xbar   = rd*tmean*3.0/skewn        
c       ta     = tmean*(1.0 - 3.0*rd/skewn) 
c
c         The equations for this distribution are given by modifying
c     the random walk function (Sheppard, 1962), substituting tau =
c     (t-ta)/xbar, and treating t as the independent variable.
c
c       F(t) = 0 for tau <= 0.0, otherwise
c
c                EXP{ -(1-tau)*(1-tau)/(k*k*tau) }
c       F(t) = -----------------------------------
c                xbar*k*tau*SQRT(pi*tau)
c
c       tau =(t-ta)/xbar; k=tkappa;
c       and the factor of (1./xbar) in the expression is because of
c       scaling t by xbar (i.e., the integral of F(t)dt = 1.)
c
c         If area <= 0.0, the set of numbers is unchanged.  If
c     area > 0.0, the set of numbers is normalized so that the integral
c     is equal to the value provided by the user.  If the area of the
c     curve is not normalized (area <= 0.0) ) it will always be the
c     same as xbar unless the curve is truncated before its end. 
c
c LIMITATIONS:
c     1.  tkappa, xbar, dt, and ndimen must be > 0.  Otherwise,
c         an error condition is returned.  
c     2.  ta must be > or = 0.0.  Otherwise an error condition is
c         returned.
c     3.  If dt >= 4.0*xbar*tkappa/SQRT(2.), a spike will be returned
c         at ta+xbar.
c     4.  The routine calculates point values, rather than integrals
c         binned into intervals and averaged for those intervals.
c         Thus, the distribution is significantly deformed even before
c         the spike condition discussed above is reached.
c     5.  The first point is always at x=0.  This is in accord with the
c         form expected by the linear interpolation routine, FINTER.
c
c REFERENCES:
c     C.W. Sheppard. Basic Principles of the Tracer Method, John Wiley
c         and Sons, Inc., New York, 1962.  pp 251,263.
c
c METHOD:
c     Direct calculation
c
c CALL AND PARAMETER TYPES:
c     INTEGER nh, ndimen
c     REAL h(ndimen), dt, frpeak, area, ta, xbar, tkappa
c     CALL ranwok(h, nh, dt, frpeak, ndimen, area, ta, xbar, tkappa)
c
c INPUTS:
c     dt        = step of independent variable
c     frpeak    = fraction below which to stop computing distribution
c     ndimen    = maximum number of points to compute
c     area      = area under the curve
c     ta        = the appearance time of the distribution
c     xbar      = mean value of unshifted distribution
c     tkappa    = the parameter for the ranwok distribution.  Somewhat
c                 similar to sigma in the Gaussian distribution.
c
c OUTPUTS:
c     h         = array of points in the distribution
c     nh        = number of points computed, or -1 if one of the
c                 limitations mentioned above is violated.
c
c SUBROUTINES CALLED:
c     expf - machine dependent exponential function.
c
c HISTORY:
c     Written:
c         01/05/88          G. Raymond
c     Modified:
c         02/13/90          S. Castaldi
c             1) Updated header.
c
c ---------------------------------------------------------------
c
      REAL h(*), dt, frpeak, area, ta, tau, xbar, tkappa
      INTEGER nh, ndimen, ih, i
      REAL aa, hmax, hcutof, squit, hi
      REAL rkappa, rksqt, coeff, sum, v
      REAL RSQPI, scale
      PARAMETER (RSQPI=0.564189583)
      REAL SQRT2
      PARAMETER( SQRT2=1.414213562)
c
c RSQPI = 1./SQRT(PI)
c
      REAL expf
      EXTERNAL expf
      CHARACTER*64 sid1,sid2
c
c Source Code Control Data
c
      DATA sid1/'@(#)ranwok.f	1.4 created 03/16/90 13:35:42.'/
      DATA sid2/'@(#) retrieved 03/31/00 22:20:52.'/
c
c
c   I.  Parameters out of limit, return error code in variable nh
c
      nh = -1
      IF( tkappa. LE. 0.) RETURN
      IF( xbar  . LE. 0.) RETURN
      IF( ta    . LT. 0.) RETURN
      IF( dt    . LE. 0.) RETURN
      IF( ndimen. LE. 0 ) RETURN
c
c  II.  Check for spike condition
c
      IF (dt. GE . 4.0*xbar*tkappa/SQRT2) THEN
          ih = NINT( (ta+xbar)/dt) + 1
          DO 5 i=1, ndimen
              h(i)=0.0
    5     CONTINUE
          IF(ih. GT. ndimen) RETURN
          h(ih) = 1./dt
          nh = ih
          IF( area. GT . 0) h(ih)=h(ih)*area
          RETURN
      END IF
c
c III.  Set proceed with direct calculation
c       A.  Set calculation cutoff
c
      hcutof = MIN(1.,frpeak)
      hcutof = MAX(0.,hcutof)
c
c       B.  Calculate ranwok
c
      h(1) = 0.0
      hmax = 0.0
      squit = -1.E07
      rkappa = 1./ (tkappa)
      scale  = RSQPI/xbar
c
      DO 10 i = 2,ndimen
          tau = (dt*REAL(i-1) - ta)/xbar
          IF( tau . GT . 0.) THEN
              rksqt = rkappa/SQRT(tau)
              coeff = rksqt*scale/tau
              v = (1.-tau)*rksqt
              hi = coeff*expf(-v*v)
              h(i) = hi
c
c     C.  Check for maximum h value and compute cutoff max(h)*frpeak
c
              IF (hi.GT.hmax) THEN
                  hmax = hi
                  squit = hmax*hcutof
              ELSE IF ((hi-squit).LE.0.0) THEN
                  nh = i
                  GO TO 20
c
              END IF
          ELSE
              h(i) = 0.0
          END IF
c
   10 CONTINUE
      nh = ndimen
   20 CONTINUE
c
c  IV.  Set area under the curve to the desired value
c
      IF( area. GT. 0.0) THEN
          sum = 0.0
          DO 30 i = 1,nh
              sum = sum + h(i)
   30     CONTINUE
          sum = sum*dt
          IF (sum.GT.0.) THEN
              aa = area/sum
          ELSE
              nh = -1
              RETURN
c
          END IF
c
          DO 40 i = 1,nh
              h(i) = h(i)*aa
   40     CONTINUE
          RETURN
      END IF
      END
