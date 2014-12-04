      SUBROUTINE expdis(h, nh, dx, ndimen, area, tau, xa)
c
c Subroutine to generate an exponential distribution
c
c From:   National Simulation Resource Facility
c         Center for Bioengineering (WD-12)
c         University of Washington
c         Seattle, WA  98195
c
c         Dr. J. B. Bassingthwaighte, Director
c
c Copyright (C) 1988, 1989, 1990 by National Simulation Resource
c Facility, Univ of WA. All Rights Reserved.
c Software may be copied so long as this copyright notice is included.
c 
c This software was developed with support from NIH grant RR-01243.
c Please cite this grant in any publication for which this software
c is used and send one reprint to the address given above.
c
c DESCRIPTION:
c     This subroutine generates an exponential distribution given
c     by (1 / tau) * exp [ -(x-xa)/tau ], from x = 0.0 to 
c     x = 8 * tau + xa, or less if limited by the input parameter
c     ndimen. 
c
c CALL AND PARAMETER TYPES:
c     INTEGER nh, ndimen
c     REAL h(ndimen), dx, area, tau, xa
c     CALL expdis(h, nh, dx, ndimen, area, tau, xa)
c
c INPUTS:
c     dx     = stepsize of independent variable (i.e. x-axis)
c     ndimen = maximum dimension of array h
c     area   = desired area under exponential curve. (Usually 1.0)
c     tau    = decay constant of the exponential.
c     xa     = appearance point of the curve
c
c OUTPUTS:
c     h      = the array of exponential distribution
c     nh     = number of points returned in array h, -1 if an error 
c              was detected and the distribution was not generated.
c
c LIMITATIONS:
c     1. Note that these distributions are computed pointwise and thus
c        one may get some absurd results if the time step is too large
c        in comparison with the sigma.  Also, the first point is at
c        zero time, in accordance with what an interpolation routine
c        like FINTER would want to see.
c     2. Computation of the area of the distribution is based on Euler
c        integration.
c     3. If tau < .25 *dx, a spike at the point closest to the mean
c        of the curve will be generated.
c     4. For absurdities such as xa or tau <= 0, the distribution is
c        not generated and nh is set to -1.
c
c SUBROUTINES CALLED:
c     None
c
c HISTORY:
c     Written:
c         ??/??/??
c     Modified:
c         09/29/87        B. Van Steenwyk
c             1) Cleaned up (much)
c         01/10/90        S. Castaldi
c             1) Brought up to current library standards. 2) Declared
c             variables.
c
c -----------------------------------------------------------------------
c
      INTEGER ndimen, nh
      REAL h(ndimen), dx, area, tau, xa
c
      INTEGER i, is, istart
      REAL aa, asum, factor, rcptau
      CHARACTER*64 SID1, SID2
c
c Source Code Control Data
c
      DATA sid1/'@(#)expdis.f	1.7 created 05/03/90 10:52:46.'/
      DATA sid2/'@(#) retrieved 03/31/00 22:20:32.'/
c
c  0. Check for spike output and absurd parameter values
c
      nh = -1
      IF ((xa.LT.0.).OR.(tau.LE.0.).OR.(dx.LE.0.))  RETURN
      IF (tau.LT.(.25*dx))  THEN
          nh = (xa+tau)/dx +1.5
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
              h(nh) = 0.
          ELSE
              nh = -1
          END IF
          RETURN
      END IF
c
c   I. Initialize parameters
c
      rcptau = 1.0 / tau
      factor = exp( -dx * rcptau)
      is     = xa / dx +.999
      istart = is + 1
      DO 10  i =1, is
          h(i) = 0.0
   10 CONTINUE
c
c  II. Calculate number of points to generate
c
      nh     = (8.0 * tau + xa) / dx + 2.
      IF (nh.GT.ndimen)  nh = ndimen
c
c III. Calculate points in the distribution (includes first point of
c      distribution:  if xa is non-integral in dx)
c
      h(istart) = rcptau *exp(-(is*dx-xa)*rcptau)
      DO 30  i = istart+1, nh
          h(i) = h(i-1)*factor
   30 CONTINUE
c
c  IV. Force the area to the area set in the call
c
      IF (area.GT.0.)  THEN
          asum = ( h(istart) -h(nh)*factor) /(1.-factor) *dx
          aa   = area/asum
          DO 45  i=istart, nh
              h(i) = h(i)*aa
   45     CONTINUE
      END IF
c
      RETURN
      END
