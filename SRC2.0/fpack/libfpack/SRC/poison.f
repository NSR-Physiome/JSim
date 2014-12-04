      SUBROUTINE poison(h, nh, dx, frpeak, ndimen, area, xbar, nlag)
c
c Subroutine to generate set of numbers using scaled Poisson density equation
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
c         This subroutine generates a set of numbers using a scaled
c     Poisson density function equation in which lambda is allowed to
c     vary continuously and n is fixed.  The set of numbers is given by
c     poison(dx*(i-1)) = (nlag/xbar) * (lambda)**n * exp(-lambda) / n!,
c     where lambda = (nlag/xbar)*dx*(i-1), n = nlag-1, and
c     i =1,2,...ndimen.  The above expression is the Poisson density
c     function equation multiplied by a constant, (nlag/xbar).  The
c     constant, (nlag/xbar) is required so that integrating the curve
c     dx is equivalent to integrating it d(lambda), and the area is
c     1.0.  This curve represents the solution in time for transport of
c     a tracer through nlag identical, uniformly mixed compartments.
c         The mean for this set of numbers equals xbar.  The standard
c     deviation for this set of numbers equals xbar/sqrt(nlag).  The
c     relative dispersion for this set of numbers equals 1./sqrt(nlag).
c     The skewness for this set of numbers equals (2/nlag**0.5)**0.5.
c     
c         If area <= 0.0, the set of numbers are unchanged.  If
c     area > 0.0, the set of numbers is normalized so that the integral
c     is equal to the value provided by the user.
c
c LIMITATIONS:
c     1.  xbar,dx, and nlag must be >0.  Otherwise, an error condition
c         is returned.
c     2.  For nlag >= (4*xbar/dx)**2, a spike is generated.
c     3.  If nlag < 4*xbar/dx)**2 and nlag too large, an error
c         condition is returned.
c     4.  If xbar/dx +1.5 > ndimen-1, then the event location of the
c         spike condition will be out of range for the output array--no
c         points will be generated.
c     5.  Note that the function is computed pointwise:  beware of
c         large values of dx.  Also, note that the first point in the
c         output array occurs at xvar equals zero.
c
c REFERENCES:
c     C.W. Sheppard. Basic Principles of the Tracer Method, John Wiley
c         and Sons, Inc., New York, 1962.  pp 190,191.
c     CRC Standard Mathematics Tables, 18th Edition, 1970, p.597.
c
c METHOD:
c     Direct calculation
c
c CALL AND PARAMETER TYPES:
c     INTEGER nh, ndimen, nlag
c     REAL h(ndimen), dx, frpeak, area, xbar
c     CALL poison(h, nh, dx, frpeak, ndimen, area, xbar, nlag)
c 
c INPUTS:
c     dx        = step of independent variable
c     frpeak    = fraction below which to stop computing distribution
c     ndimen    = maximum number of points to compute
c     area      = area under the curve
c     xbar      = independent variable expectation value for this curve
c     nlag      = number of identical ideally mixed compartments in series
c
c OUTPUTS:
c     h         = array of points in the distribution
c     nh        = number of points computed, or -1 if one of the limitations
c                 mentioned above is violated.
c
c SUBROUTINE CALLED:
c     rfactr (function generating a factorial of given integer number)
c
c HISTORY:
c     Written:
c         ??/??/??
c     Modified:
c         12/04/87    B. Van Steenwyk
c             1) Rewritten.
c         02/17/88    G. Raymond
c             1) Documentation revised to reflect the use of a Poisson
c             distribution equation and improve speed. 
c         05/05/89    L. Weissman
c             1) Syntax changes, restrict range of np.
c         02/13/90    S. Castaldi
c             1) Updated header. 2) Declared variables.
c
c -----------------------------------------------------------------------
c
      INTEGER i, nh, ndimen, nlag
      REAL h(*), dx, frpeak, area, xbar
c
      INTEGER n
      REAL aa, cm, const, const2, expon, squit, sum, temp, xvar
      CHARACTER*64 sid1, sid2
      REAL rfactr
      EXTERNAL rfactr
c
c Source Code Control Data
c
      DATA sid1/'@(#)poison.f	1.5 created 02/13/90 15:31:26.'/
      DATA sid2/'@(#) retrieved 03/31/00 22:20:50.'/
c
c . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
c
c 0.  Parameters out of limit, return spike when appropriate
c
      nh     = -1
      IF( (dx.LE.0.).OR.(xbar.LE.0.).OR.(nlag.LE.0) ) RETURN
      IF (nlag.GE.( (4.*xbar/dx)**2) )  THEN
          nh  = xbar/dx +1.5
          IF (nh.GT.(ndimen-1))  THEN
              nh = -1
              RETURN
          ENDIF
          DO 5 i=1, nh
              h(i)  = 0.
    5     CONTINUE
          IF (area.GT.0.)  THEN
              h(nh) = area/dx
          ELSE
              h(nh) = 1./dx
          ENDIF
          nh    = nh +1
          h(nh) = 0.
          RETURN
      ENDIF
      const  = rfactr(nlag-1)
      IF (const .LE. 0.0) RETURN
c
c I.  Setting Up  (also, check for some set-up errors)
c
      temp   = float(nlag)/xbar
      n      = nlag-1
      const  = temp/const
      temp   = temp*dx
      const2 = exp(-dx/xbar)
      expon  = 1.
      cm     = 0.0
      xvar   = 0.0
c
c      A.  If n equals zero, then set xvar equal one. (anything)**0
c          equals 1, except 0**0, which returns an error in FORTRAN.    
c
      IF(n.LE.0) xvar=1.0
c
c II. compute h(i) at each dx  
c
      DO 20 i = 1,ndimen
c
c     A.  Direct computation
c
          h(i)  = const*(expon*xvar)**n
          xvar  = xvar + temp
          const = const*const2
          expon = expon*const2
c
c     B.  Check for maximum h value and compute cutoff max(h)*frpeak
c
          IF (h(i).GE.cm) THEN
              cm = h(i)
              squit = frpeak*cm
c
c     C.  Stop calculation when h(i) < max(h)*frpeak and set nh
c
          ELSE IF (h(i) .LE. squit)  THEN
              nh = i 
              GOTO 30 
          ENDIF
c
   20    CONTINUE
c
c III. Set nh if not already
c
   30 CONTINUE
      IF (nh.EQ.-1)  nh = ndimen
c
c IV. Set area under the curve to the desired value
c
      IF (area.GT.0.0)  THEN
          sum     = 0.0
          DO 40  i =1, nh
              sum  = sum + h(i)
   40     CONTINUE
          sum     = sum * dx
          IF (sum.GT.0.)  THEN
              aa  = area / sum
          ELSE
              nh  = -1
              RETURN
          ENDIF
          DO 45  i =1, nh
              h(i) = h(i) * aa
   45     CONTINUE
       ENDIF
c
      RETURN
      END
