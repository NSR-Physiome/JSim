      SUBROUTINE lagndc(h, nh, dx, frpeak, ndimen, area, sigma, tau,
     +                  xc, ix)
c
c Subroutine generates a lagged normal density function.
c
c File lagndc.f (Version 1.15).  Last modified 16:02:29, 08/11/95.
c
c.......................................................................
c
c From:  National Simulation Resource 
c        Center for Bioengineering
c        University of Washington
c        Box 357962
c        Seattle, WA 98195-7962
c
c        Dr. J. B. Bassingthwaighte, Director
c
c.......................................................................
c
c Copyright (C) 1988 - 1995 by National Simulation Resource, Univ of WA.
c All Rights Reserved.
c 
c Software may be copied so long as this copyright notice is included.
c This software was developed with support from NIH grant RR-01243.
c Please cite this grant in any publication for which this software
c is used and send one reprint to the address given above.
c
c.......................................................................
c
c DESCRIPTION:
c
c         This subroutine computes a lagged normal density curve
c     (LAGNDC), described in Bassingthwaighte et al. (1966).  The fact
c     that an LAGNDC is the convolution of an exponential with time
c     constant tau and a Gaussian is used to advantage in the
c     computation:
c
c     LAGNDC = exp(-t/tau) / (tau*sig*sqrt(2*PI))
c         * integral {0 -> t} exp(a/tau) * exp(-.5*(a-xc)**2/sig**2) da
c
c     The integral:
c     integral {0 -> t} exp(a/tau) * exp(-.5*(a-xc)**2/sig**2) da
c     is equal to:  exp(xc/tau + sig**2/(2*tau**2)) * sqrt(2*PI) * sig
c          * [ Prf((t - (xc+sig**2/tau))/sig) - Prf(-xc/sig -sig/tau) ]
c
c     The function Prf() is the standard probability density function
c 
c     Prf(x) = 1./SQRT(2.*pi) * integral{-infinity ->x} exp(-t*t/2) dt.
c
c     While special scaling is required to avoid multiplying zeros and
c     infinities, the algorithm is quite stable.  If an instability
c     is encountered, the output parameter nh is set negative to
c     indicate an error condition.  The information used for the
c     rational approximation comes from Abramowitz and Stegun (1968).
c
c         To increase the usefulness of this routine as a model of the
c     transport function in the cardiovascular system as described on
c     p.882 of Bassingthwaighte and Ackerman (1967), an option to
c     linearize the up slope of the distribution is included.
c
c
c LIMITATIONS:
c
c     Under some conditions, alternate functions are used.
c
c     If tau < sigma/40, a Gaussian density function is used.
c     If sigma < tau/50, an exponential density function is used.
c     If sqrt(sigma**2 + tau**2) < dx/4, a spike at (xc+tau) is
c     returned.
c
c     Remember that a point function is calculated, and absurd results
c     can happen before the spike condition is reached.
c     Note that the first array position holds the value at time = 0.
c     Note that the curve will stop at 1.e-25*peak value and no smaller
c     values will be calculated.
c 
c
c REFERENCES:
c
c         J.B. Bassingthwaighte, F.H. Ackerman, and E.H. Wood.
c     Applications of a lagged normal density curve as a model for
c     arterial dilution curves.  Circ. Res. 18:398-415, 1966.
c         J.B. Bassingthwaighte and F.H. Ackerman.  Mathematical
c     linearity of circulatory transport.  J. Appl. Physiol.
c     22:879-888, 1967.
c         M. Abramowitz and I.A. Stegun.  Handbook of Mathematical
c     Functions, Dover, 1968, p932.
c     (NOTE: The rational approximation in Article 26.2.17 is used with
c            the coefficients multiplied by ( 1./SQRT(2.*pi) ).
c
c
c CALL AND PARAMETER TYPES:
c
c     INTEGER ix, ndimen, nh
c     REAL    h(ndimen), dx, frpeak, area, sigma, tau, xc
c     CALL    lagndc(h,nh,dx,frpeak,ndimen,area,sigma,tau,xc,ix)
c
c
c INPUTS:
c
c     dx     - The interval between the independent variables.
c     frpeak - The fraction of peak height below which the curve is
c              not to be computed.  (Usually set to 0.01)
c     ndimen - The maximum number of points to be computed.
c     area   - The desired area of the LNDC. (Usually 1.0) Note: if
c              area .LE. 0, the curve will not be normalized.
c     sigma  - The standard deviation of the normal curve.
c     tau    - The decay constant of the exponential.
c     xc     - The mean of the normal curve.
c     ix     - A flag for linearization of the upslope of the curve
c              below 40% of the peak height.
c                 ix = 0     for normal LNDC.
c                 ix .NE. 0  for a linear upslope.
c
c
c OUTPUTS:
c
c     h      - A real array containing the computed values of the lndc.
c              <Dimensioned (ndimen or greater) in the mainline.>
c     nh     - The number of points computed in c.
c              Also used to indicate error conditions
c              -1 = Unacceptable values encountered in setup stage
c              -2 = Numerical values of some terms are unacceptably
c                   large, and the calculation is terminated. 
c
c
c SUBROUTINES CALLED:
c
c     expdis  - exponential distribution routine
c     gaudis  - Gaussian distribution routine
c     expf    - compute exp function with machine limits
c
c
c HISTORY:
c
c     Written:
c         /  /1966          Frank Ackerman
c     Modified:
c         /  /1983          Marta Chaloupka
c         Test program written
c
c         /  /1985          Robin Budd
c         Ported and tested for MV/10000
c
c         /  /1986          R. King
c         Added code for check and generation of a spike
c
c         /  /1987          B. van Steenwyk
c         New computation scheme.
c
c         02/09/89          G. Raymond
c             1) Revised to current standards.  2) Allows both tau and
c             sigma to equal zero without an error.  3) Main 
c             computational loop rewritten and plethora of if tests 
c             removed.  4) Code corrected to allow for linearized 
c             upslope to be calculated.  5) Code added to compute the
c             exponential tail when the terms become exponential (i.e.
c             failing off each time step by exp(-dx/tau). )  
c             6) Additional documentation added.
c
c         02/13/90          S. Castaldi
c             1) Changed continuation symbol.
c         05/25/90          S. Castaldi
c             1) Changed declaration of h from ndimen to *.  2) Added
c             zero point after spike. 3) Added calculations to limit
c             ndim in accordance with frpeak when calling expdis and
c             gaudis.
c
c         ver. 1.15  W. Chan (AUG95)
c             Changed termination value from 1.0e-07 to 1.0e-25 of peak.
c
c----------------------------------------------------------------------
c
      CHARACTER*64 sid1, sid2
      INTEGER nh, ndimen, ix
      REAL h(*), dx, frpeak, area, sigma, tau, xc
      
c
      REAL cutoff
c
c Constants for use in the Prf() calculations.
c
      REAL A1, A2, A3, A4, A5, SCALER
      PARAMETER (A1=0.1274147959, A2=-0.1422483683,
     +           A3=0.7107068705, A4=-0.7265760129,
     +           A5=0.5307027141, SCALER= 0.2316419 )
c
      INTEGER i,j, ncut, ndo, itop, ibot, imax, ndim
      REAL scalin, dxdeca, sigmdx, basetx, holder, zeroef, prft
      REAL x, y, expdec, exptot, areasu, pkvalu, pkcut
      REAL htop, hbot, decr, rlpeak
      REAL gausig, gaumen
      REAL xpoly, s, expf
      EXTERNAL gaudis, expdis, expf  
c
c Source Code Control Data
c
      DATA sid1/'@(#)lagndc.f	1.15 created 08/11/95 16:02:29.'/
      DATA sid2/'@(#) retrieved 03/31/00 22:20:41.'/
c
c IN line function defined: xpoly is the polynomial part of Formula
c 26.2.17 in Abramowitz and Stegun, 1964, where the coefficients have
c been multiplied by 1/SQRT(2.*pi) to speed the computation
c
      xpoly(s)=s*(A1+s*(A2+s*(A3+s*(A4+s*A5))))
c
c   0.  Check for special cases.
c
c     A. The absurd cases
c
      cutoff = MIN(1.0,frpeak)
      cutoff = MAX(cutoff, 1.0E-25)
      rlpeak = LOG(cutoff)
      nh = -1
      IF ((dx .LE. 0.) .OR. (xc .LE. 0.) .OR. (sigma .LT. 0.) .OR.
     +    (tau .LT. 0.) .OR. (ndimen.LE.1) ) RETURN
c
c      B. If sqrt(sigma**2 + tau**2) < dx/4, a spike at (xc+tau) is
c         returned.
c
      IF ((sigma*sigma + tau*tau) .LT. (0.0625*dx*dx)) THEN
          nh = NINT ( (xc + tau)/ dx)  + 1
          IF (nh .GT. (ndimen - 1)) THEN
              nh = -1
              RETURN
          ENDIF
c
          DO 10 i = 1, nh
              h(i) = 0.0
   10     CONTINUE
          IF (area .GT. 0.) THEN
              h(nh) = area / dx
          ELSE
              h(nh) = 1.0 / dx
          ENDIF
c
c         Add zero point after spike.
c
          nh = nh + 1
          h(nh) = 0.0
c
          RETURN
c
c     C.  If tau < sigma/40, use a Gaussian density function.
c
      ELSE IF (tau .LT. 0.025*sigma) THEN
          gausig = SQRT(sigma*sigma+tau*tau)
          gaumen = xc+tau
          ndim = INT((SQRT(-2*gausig*gausig*rlpeak) + gaumen) / dx)
          ndim = MIN(ndim,ndimen)
          CALL gaudis(h, nh, dx, ndim, area, gausig, gaumen)
          RETURN
c
c     D.  If sigma < tau/50, use an exponential density function.
c
      ELSE IF (sigma .LT. 0.02*tau) THEN
          ndim = INT( (-tau * rlpeak + xc) / dx)
          ndim = MIN(ndim,ndimen)
          CALL expdis(h, nh, dx, ndim, area, tau, xc)
          RETURN
c
      ENDIF
c
c   I.  Calculate the constants used in the computations.
c       Note: the calculation is not done in the most straightforward
c       fashion that might be expected.  In the actual formula, there
c       are a number of exponential terms which individually may 
c       exceed the largest floating point number available.  By
c       combining the exponential terms before the exponential operator
c       is used, this numerical problem can be avoided.
c
      scalin = 0.5*sigma*sigma/(tau*tau) + xc/tau
      dxdeca = dx/tau
      sigmdx = dx/sigma
      basetx = -(xc/sigma + sigma/tau)
      expdec = expf(-dxdeca)
      exptot = 1.
c
c The cutoff is calculated when the lag normal density curve can be
c calculated by a simple multiplication because it has become
c monoexponential.  The cutoff occurs when 
c Pr(t/sigma -xc/sigma -sigma/tau) -> 1.0.  Since
c Pr(x) = 1-exp(-x*x/2)*xpoly(1/(1+p*x)), the cut off has been chosen
c when -x*x/2 = -18.0, or x=+6, at which point Pr(x) = 1. 
c ncut = sigma/dx*(6.+xc/sigma+sigma/tau)
c
      ncut = (6.-basetx)/sigmdx+4
c
c +4 is added to insure that the computation occurs after the maximum
c value of the curve.
c
      ndo = MIN(ncut,ndimen)
c
c  II.  Compute the weighted, zero error function value.
c
      holder = 1./(1. - SCALER*basetx)
      x      = scalin - 0.5*basetx*basetx
      zeroef = -expf(x)*xpoly(holder)
c
c III.  Main loop to compute points of the integral.
c
      areasu = 0.0
      pkvalu = 0.0
      pkcut  = 0.0
      h(1)   = 0.0
c
      DO 30 i=2,ndo  
          exptot = exptot*expdec
c
c     A.  Present value of decay times.
c
          scalin = scalin - dxdeca
          basetx = basetx + sigmdx
          holder = 1./(1. + SCALER*abs(basetx))
c
c     B.  Compute the weighted, modified Prf().
c
          x = scalin - 0.5*basetx*basetx
          IF (x .GT. 25.) THEN
              nh = -2
              RETURN
          ELSE
              prft   = expf(x)*xpoly(holder)
          ENDIF
c
c     C.  Take the appropriate difference between this Prf() 
c         and the base Prf().
c
          IF (basetx .gt. 0.0) THEN
              h(i) = (exptot*zeroef - prft   + expf(scalin))/tau
          ELSE
              h(i) = ( exptot*zeroef + prft )/tau  
          ENDIF
c
c
c     D.  Maintain the peak value while looking for the cutoff.
c
          IF (pkvalu .LT. h(i)) THEN
              imax = i
              pkvalu = h(i)
              pkcut = pkvalu * cutoff
          ELSE IF (pkcut .GT. h(i)) THEN
              nh = i
              GO TO 40
          ENDIF
c
   30 CONTINUE
c
c  IV.  Loop for exponential tail
c
      DO 31 i=ndo,ndimen
          h(i)=h(i-1)*expdec
c
          IF (pkvalu .LT. h(i)) THEN
              imax = i
              pkvalu = h(i)
              pkcut = pkvalu * cutoff
          ELSE IF (pkcut .GT. h(i)) THEN
              nh = i
              GO TO 40
c
          ENDIF
   31 CONTINUE
c
c   V.  Come here when cutoff has occurred or ndimen has been reached.
c
      nh = ndimen
   40 CONTINUE
c
c  VI.  Check to see if the front end of the curve is to be linearized.
c
      IF (ix .NE. 0) THEN
c
c     A.  Find the points through which to draw a straight line.
c
          htop = 0.80 * pkvalu
          hbot = 0.40 * pkvalu
c
          imax = MAX(2,imax)
          DO 50 i = imax - 1, 1, -1
              IF (h(i) .LE. htop) GO TO 51
   50     CONTINUE
   51     htop = h(i)
          itop = i
c
          DO 52 i = itop - 1, 1, -1
              IF (h(i) .LE. hbot) GO TO 53
   52     CONTINUE
   53     hbot = h(i)
          ibot = i
c
c     B.  Calculate the change in h(i) per dx.
c
          decr = (htop - hbot) / REAL(itop - ibot)
c
c     C.  Extrapolate to the baseline and zero earlier points.
c
          IF (ibot .GT. 1) THEN
              DO 55 i = ibot - 1, 1, -1
                  h(i) = h(i + 1) - decr
                  IF (h(i) .LT. 0.0) THEN
                      DO 54 j = i, 1, -1
                          h(j) = 0.0
   54                 CONTINUE
                      GO TO 60
                  ENDIF
   55         CONTINUE
          ENDIF
c
      ENDIF
c
c VII.  Force the area to the area set in the call.
c
   60 CONTINUE
      IF (area .GT. 0.0) THEN
          DO 61 i=1, nh
              areasu=areasu+h(i)
   61     CONTINUE
          IF (areasu .LE. 0.) THEN
              nh = -1
              RETURN
          ENDIF
c
          y = area / (areasu*dx)
          DO 62 i = 1, nh
              h(i) = h(i) * y
   62     CONTINUE
      ENDIF
c
      RETURN
      END
